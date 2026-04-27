package controlplane

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"math"
	"math/big"
	"net/http"
	"strconv"
	"strings"
	"time"
)

const (
	erc20TransferTopic      = "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
	usdcWatcherMaxBlockSpan = uint64(2_000)
)

type USDCWatcherConfig struct {
	RPCURL          string
	Network         string
	ChainID         string
	TokenAddress    string
	ReceiveAddress  string
	OrgID           string
	WatchInterval   time.Duration
	Confirmations   uint64
	StartBlock      uint64
	ExplorerBaseURL string
}

type USDCWatcher struct {
	store  *Store
	cfg    USDCWatcherConfig
	client *ethRPCClient
}

func NewUSDCWatcher(store *Store, cfg USDCWatcherConfig) *USDCWatcher {
	return &USDCWatcher{
		store:  store,
		cfg:    normalizeUSDCWatcherConfig(cfg),
		client: newEthRPCClient(cfg.RPCURL),
	}
}

func (w *USDCWatcher) Run(ctx context.Context) {
	if w == nil || w.store == nil {
		return
	}
	if !w.enabled() {
		_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(false, 0, 0, 0, ""))
		return
	}
	initialLast := uint64(0)
	if w.cfg.StartBlock > 0 {
		initialLast = w.cfg.StartBlock - 1
	}
	_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, initialLast, 0, 0, ""))
	defer func() {
		health, err := w.store.USDCWatcherHealth(context.Background(), w.cfg.Network)
		if err != nil {
			health = w.health(false, initialLast, 0, 0, "")
		}
		health.Enabled = w.enabled()
		health.Running = false
		health.LastCheckedAt = nowString()
		_ = w.store.UpdateUSDCWatcherHealth(context.Background(), health)
	}()

	ticker := time.NewTicker(w.cfg.WatchInterval)
	defer ticker.Stop()
	for {
		if err := w.poll(ctx); err != nil && ctx.Err() == nil {
			slog.Warn("usdc watcher poll failed", "network", w.cfg.Network, "err", err)
		}
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
		}
	}
}

func VerifyUSDCDepositTx(ctx context.Context, cfg USDCWatcherConfig, dep USDCDeposit) (USDCDetectedDeposit, error) {
	cfg = normalizeUSDCWatcherConfig(cfg)
	if strings.TrimSpace(cfg.RPCURL) == "" {
		return USDCDetectedDeposit{}, fmt.Errorf("SQUASH_BASE_RPC_URL is not configured")
	}
	if err := ValidateManualUSDCDeposit(dep, cfg.Network, cfg.TokenAddress, cfg.ReceiveAddress); err != nil {
		return USDCDetectedDeposit{}, err
	}
	client := newEthRPCClient(cfg.RPCURL)
	if err := verifyChainID(ctx, client, cfg.ChainID); err != nil {
		return USDCDetectedDeposit{}, err
	}
	latest, err := client.blockNumber(ctx)
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	receipt, err := client.transactionReceipt(ctx, dep.TxHash)
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	if receipt == nil {
		return USDCDetectedDeposit{}, fmt.Errorf("transaction receipt not found")
	}
	if receipt.Status != "" && receipt.Status != "0x1" {
		return USDCDetectedDeposit{}, fmt.Errorf("transaction was not successful")
	}
	if !validTxHash(receipt.TransactionHash) || !strings.EqualFold(receipt.TransactionHash, dep.TxHash) {
		return USDCDetectedDeposit{}, fmt.Errorf("transaction hash mismatch")
	}
	for _, log := range receipt.Logs {
		verified, err := verifiedDepositFromLog(log, latest, cfg)
		if err != nil {
			continue
		}
		if verified.TxHash != "" && !strings.EqualFold(verified.TxHash, dep.TxHash) {
			continue
		}
		if dep.FromAddress != "" && !strings.EqualFold(verified.FromAddress, dep.FromAddress) {
			continue
		}
		if verified.AmountMicros == dep.AmountMicros {
			return verified, nil
		}
	}
	return USDCDetectedDeposit{}, fmt.Errorf("no verified USDC transfer matched tx hash, token, recipient, amount, network, and confirmation depth")
}

func (w *USDCWatcher) poll(ctx context.Context) error {
	if err := verifyChainID(ctx, w.client, w.cfg.ChainID); err != nil {
		_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, 0, 0, 0, err.Error()))
		return err
	}
	latest, err := w.client.blockNumber(ctx)
	if err != nil {
		_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, 0, 0, 0, err.Error()))
		return err
	}
	confirmed := confirmedBlock(latest, w.cfg.Confirmations)
	last, ok, err := w.store.USDCWatcherCursor(ctx, w.cfg.Network)
	if err != nil {
		_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, last, latest, confirmed, err.Error()))
		return err
	}
	if !ok {
		last = 0
		if w.cfg.StartBlock > 0 {
			last = w.cfg.StartBlock - 1
		}
	}
	if confirmed <= last {
		return w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, last, latest, confirmed, ""))
	}
	to := confirmed
	if to-last > usdcWatcherMaxBlockSpan {
		to = last + usdcWatcherMaxBlockSpan
	}
	logs, err := w.client.logs(ctx, ethLogFilter{
		Address:   w.cfg.TokenAddress,
		FromBlock: hexUint64(last + 1),
		ToBlock:   hexUint64(to),
		Topics:    []any{erc20TransferTopic, nil, addressTopic(w.cfg.ReceiveAddress)},
	})
	if err != nil {
		_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, last, latest, confirmed, err.Error()))
		return err
	}
	for _, log := range logs {
		verified, err := verifiedDepositFromLog(log, latest, w.cfg)
		if err != nil {
			_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, last, latest, confirmed, err.Error()))
			return err
		}
		if err := w.creditVerifiedDeposit(ctx, verified); err != nil {
			_ = w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, last, latest, confirmed, err.Error()))
			return err
		}
	}
	return w.store.UpdateUSDCWatcherHealth(ctx, w.health(true, to, latest, confirmed, ""))
}

func (w *USDCWatcher) creditVerifiedDeposit(ctx context.Context, dep USDCDetectedDeposit) error {
	recorded, err := w.store.RecordUSDCDetectedDeposit(ctx, dep)
	if err != nil {
		return err
	}
	dep = recorded.Deposit
	if !recorded.Inserted && dep.Credited {
		return nil
	}
	if dep.DuplicateTx {
		return w.store.MarkUSDCDetectedDeposit(ctx, dep.TxHash, dep.LogIndex, false, "duplicate_tx", "")
	}
	result, err := w.store.ConfirmUSDCDeposit(ctx, USDCDeposit{
		OrgID:        dep.OrgID,
		TxHash:       dep.TxHash,
		AmountMicros: dep.AmountMicros,
		Network:      dep.Network,
		TokenAddress: dep.TokenAddress,
		FromAddress:  dep.FromAddress,
		ToAddress:    dep.ToAddress,
	})
	if err != nil {
		_ = w.store.MarkUSDCDetectedDeposit(ctx, dep.TxHash, dep.LogIndex, false, "error", err.Error())
		return err
	}
	if !result.Credited {
		return w.store.MarkUSDCDetectedDeposit(ctx, dep.TxHash, dep.LogIndex, false, "duplicate_tx", "")
	}
	return w.store.MarkUSDCDetectedDeposit(ctx, dep.TxHash, dep.LogIndex, true, "credited", "")
}

func (w *USDCWatcher) enabled() bool {
	return strings.TrimSpace(w.cfg.RPCURL) != "" &&
		strings.TrimSpace(w.cfg.TokenAddress) != "" &&
		strings.TrimSpace(w.cfg.ReceiveAddress) != ""
}

func (w *USDCWatcher) health(running bool, last, latest, confirmed uint64, lastErr string) USDCWatcherHealth {
	lastSuccess := ""
	if lastErr == "" && latest > 0 {
		lastSuccess = nowString()
	}
	return USDCWatcherHealth{
		Network:                  w.cfg.Network,
		Enabled:                  w.enabled(),
		Running:                  running,
		RPCURLConfigured:         strings.TrimSpace(w.cfg.RPCURL) != "",
		ReceiveAddressConfigured: strings.TrimSpace(w.cfg.ReceiveAddress) != "",
		LastScannedBlock:         last,
		LatestBlock:              latest,
		ConfirmedBlock:           confirmed,
		Confirmations:            w.cfg.Confirmations,
		WatchIntervalSeconds:     int64(w.cfg.WatchInterval.Seconds()),
		LastCheckedAt:            nowString(),
		LastSuccessAt:            lastSuccess,
		LastError:                lastErr,
	}
}

func normalizeUSDCWatcherConfig(cfg USDCWatcherConfig) USDCWatcherConfig {
	if cfg.Network == "" {
		cfg.Network = "base"
	}
	if cfg.ChainID == "" {
		cfg.ChainID = "8453"
	}
	if cfg.WatchInterval <= 0 {
		cfg.WatchInterval = 30 * time.Second
	}
	if cfg.Confirmations == 0 {
		cfg.Confirmations = 12
	}
	if cfg.OrgID == "" {
		cfg.OrgID = DefaultOrgID
	}
	cfg.TokenAddress = strings.TrimSpace(cfg.TokenAddress)
	cfg.ReceiveAddress = strings.TrimSpace(cfg.ReceiveAddress)
	return cfg
}

func verifiedDepositFromLog(log ethLog, latest uint64, cfg USDCWatcherConfig) (USDCDetectedDeposit, error) {
	if !validTxHash(log.TransactionHash) {
		return USDCDetectedDeposit{}, fmt.Errorf("invalid tx hash in USDC log")
	}
	if !strings.EqualFold(log.Address, cfg.TokenAddress) {
		return USDCDetectedDeposit{}, fmt.Errorf("USDC token contract mismatch")
	}
	if len(log.Topics) < 3 || !strings.EqualFold(log.Topics[0], erc20TransferTopic) {
		return USDCDetectedDeposit{}, fmt.Errorf("not an ERC20 Transfer log")
	}
	to, err := addressFromTopic(log.Topics[2])
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	if !strings.EqualFold(to, cfg.ReceiveAddress) {
		return USDCDetectedDeposit{}, fmt.Errorf("USDC recipient mismatch")
	}
	from, err := addressFromTopic(log.Topics[1])
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	amount, err := uint256ToInt64(log.Data)
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	if amount <= 0 {
		return USDCDetectedDeposit{}, fmt.Errorf("USDC transfer amount must be positive")
	}
	blockNumber, err := parseHexUint64(log.BlockNumber)
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	logIndex, err := parseHexUint64(log.LogIndex)
	if err != nil {
		return USDCDetectedDeposit{}, err
	}
	if blockNumber > latest {
		return USDCDetectedDeposit{}, fmt.Errorf("USDC log block is ahead of latest block")
	}
	depth := latest - blockNumber + 1
	if depth < cfg.Confirmations {
		return USDCDetectedDeposit{}, fmt.Errorf("USDC transfer has %d confirmations, need %d", depth, cfg.Confirmations)
	}
	return USDCDetectedDeposit{
		OrgID:         cfg.OrgID,
		TxHash:        strings.ToLower(log.TransactionHash),
		LogIndex:      logIndex,
		BlockNumber:   blockNumber,
		BlockHash:     log.BlockHash,
		Network:       cfg.Network,
		TokenAddress:  cfg.TokenAddress,
		FromAddress:   from,
		ToAddress:     cfg.ReceiveAddress,
		AmountMicros:  amount,
		Confirmations: depth,
		Status:        "detected",
		DetectedAt:    nowString(),
	}, nil
}

func verifyChainID(ctx context.Context, client *ethRPCClient, expected string) error {
	got, err := client.chainID(ctx)
	if err != nil {
		return err
	}
	if !chainIDMatches(got, expected) {
		return fmt.Errorf("network chain_id mismatch: got %s, want %s", got, expected)
	}
	return nil
}

func confirmedBlock(latest, confirmations uint64) uint64 {
	if confirmations <= 1 {
		return latest
	}
	if latest+1 <= confirmations {
		return 0
	}
	return latest - confirmations + 1
}

type ethRPCClient struct {
	url        string
	httpClient *http.Client
	nextID     int
}

func newEthRPCClient(url string) *ethRPCClient {
	return &ethRPCClient{
		url: strings.TrimSpace(url),
		httpClient: &http.Client{
			Timeout: 20 * time.Second,
		},
	}
}

func (c *ethRPCClient) chainID(ctx context.Context) (string, error) {
	var out string
	if err := c.call(ctx, "eth_chainId", []any{}, &out); err != nil {
		return "", err
	}
	return out, nil
}

func (c *ethRPCClient) blockNumber(ctx context.Context) (uint64, error) {
	var out string
	if err := c.call(ctx, "eth_blockNumber", []any{}, &out); err != nil {
		return 0, err
	}
	return parseHexUint64(out)
}

type ethLogFilter struct {
	Address   string `json:"address"`
	FromBlock string `json:"fromBlock"`
	ToBlock   string `json:"toBlock"`
	Topics    []any  `json:"topics"`
}

func (c *ethRPCClient) logs(ctx context.Context, filter ethLogFilter) ([]ethLog, error) {
	var out []ethLog
	if err := c.call(ctx, "eth_getLogs", []any{filter}, &out); err != nil {
		return nil, err
	}
	return out, nil
}

func (c *ethRPCClient) transactionReceipt(ctx context.Context, txHash string) (*ethReceipt, error) {
	raw, err := c.callRaw(ctx, "eth_getTransactionReceipt", []any{txHash})
	if err != nil {
		return nil, err
	}
	if bytes.Equal(bytes.TrimSpace(raw), []byte("null")) {
		return nil, nil
	}
	var out ethReceipt
	if err := json.Unmarshal(raw, &out); err != nil {
		return nil, err
	}
	return &out, nil
}

func (c *ethRPCClient) call(ctx context.Context, method string, params any, out any) error {
	raw, err := c.callRaw(ctx, method, params)
	if err != nil {
		return err
	}
	return json.Unmarshal(raw, out)
}

func (c *ethRPCClient) callRaw(ctx context.Context, method string, params any) (json.RawMessage, error) {
	if c.url == "" {
		return nil, fmt.Errorf("rpc url is not configured")
	}
	c.nextID++
	payload, err := json.Marshal(rpcRequest{
		JSONRPC: "2.0",
		ID:      c.nextID,
		Method:  method,
		Params:  params,
	})
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, c.url, bytes.NewReader(payload))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	res, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	body, err := io.ReadAll(io.LimitReader(res.Body, 4<<20))
	if err != nil {
		return nil, err
	}
	if res.StatusCode < 200 || res.StatusCode >= 300 {
		return nil, fmt.Errorf("rpc %s failed: HTTP %d", method, res.StatusCode)
	}
	var decoded rpcResponse
	if err := json.Unmarshal(body, &decoded); err != nil {
		return nil, err
	}
	if decoded.Error != nil {
		return nil, fmt.Errorf("rpc %s error %d: %s", method, decoded.Error.Code, decoded.Error.Message)
	}
	return decoded.Result, nil
}

type rpcRequest struct {
	JSONRPC string `json:"jsonrpc"`
	ID      int    `json:"id"`
	Method  string `json:"method"`
	Params  any    `json:"params"`
}

type rpcResponse struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int             `json:"id"`
	Result  json.RawMessage `json:"result"`
	Error   *rpcError       `json:"error,omitempty"`
}

type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

type ethReceipt struct {
	TransactionHash string   `json:"transactionHash"`
	BlockNumber     string   `json:"blockNumber"`
	Status          string   `json:"status"`
	Logs            []ethLog `json:"logs"`
}

type ethLog struct {
	Address         string   `json:"address"`
	Topics          []string `json:"topics"`
	Data            string   `json:"data"`
	BlockNumber     string   `json:"blockNumber"`
	BlockHash       string   `json:"blockHash"`
	TransactionHash string   `json:"transactionHash"`
	LogIndex        string   `json:"logIndex"`
}

func parseHexUint64(raw string) (uint64, error) {
	s := strings.TrimPrefix(strings.ToLower(strings.TrimSpace(raw)), "0x")
	if s == "" {
		return 0, fmt.Errorf("empty hex quantity")
	}
	return strconv.ParseUint(s, 16, 64)
}

func hexUint64(v uint64) string {
	return fmt.Sprintf("0x%x", v)
}

func chainIDMatches(got, want string) bool {
	got = strings.ToLower(strings.TrimSpace(got))
	want = strings.ToLower(strings.TrimSpace(want))
	if want == "" {
		return true
	}
	gotN, err := parseHexUint64(got)
	if err != nil {
		return false
	}
	if strings.HasPrefix(want, "0x") {
		wantN, err := parseHexUint64(want)
		return err == nil && gotN == wantN
	}
	return strconv.FormatUint(gotN, 10) == want
}

func addressTopic(address string) string {
	addr := strings.TrimPrefix(strings.ToLower(strings.TrimSpace(address)), "0x")
	if len(addr) >= 64 {
		return "0x" + addr
	}
	return "0x" + strings.Repeat("0", 64-len(addr)) + addr
}

func addressFromTopic(topic string) (string, error) {
	s := strings.TrimPrefix(strings.ToLower(strings.TrimSpace(topic)), "0x")
	if len(s) != 64 {
		return "", fmt.Errorf("invalid address topic")
	}
	return "0x" + s[24:], nil
}

func uint256ToInt64(raw string) (int64, error) {
	s := strings.TrimPrefix(strings.ToLower(strings.TrimSpace(raw)), "0x")
	if s == "" {
		return 0, fmt.Errorf("empty uint256")
	}
	n := new(big.Int)
	if _, ok := n.SetString(s, 16); !ok {
		return 0, fmt.Errorf("invalid uint256")
	}
	if n.BitLen() > 63 || n.Cmp(big.NewInt(math.MaxInt64)) > 0 {
		return 0, fmt.Errorf("uint256 does not fit int64")
	}
	return n.Int64(), nil
}

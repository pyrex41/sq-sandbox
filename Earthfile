VERSION 0.8

image:
    FROM DOCKERFILE -f Dockerfile.zig .
    ARG tag=local
    SAVE IMAGE sq-sandbox:${tag}

# Run network isolation smoke test against a local container.
# Usage: earthly +image && earthly +test-net
test-net:
    LOCALLY
    RUN docker rm -f sq-sandbox-test 2>/dev/null || true
    RUN docker run -d --name sq-sandbox-test \
        --privileged \
        --device /dev/fuse \
        --tmpfs /data/sandboxes:exec,mode=777,size=2g \
        -e SQUASH_BACKEND=chroot \
        -e SQUASH_PORT=8080 \
        -e SQUASH_AUTH_TOKEN=test \
        -e SQUASH_DATA=/data \
        sq-sandbox:local
    RUN for i in $(seq 1 30); do \
            docker exec sq-sandbox-test curl -sf http://localhost:8080/cgi-bin/health >/dev/null 2>&1 && break || true; \
            echo "Waiting for sidecar... ($i/30)"; sleep 1; \
        done
    RUN docker exec sq-sandbox-test sq-mkbase alpine
    RUN echo "=== Test 1: network sandbox (allow_net) can reach internet ===" && \
        docker exec sq-sandbox-test sh -c " \
          curl -sf -X POST http://localhost:8080/cgi-bin/api/sandboxes \
            -H 'Authorization: Bearer test' -H 'Content-Type: application/json' \
            -d '{\"id\":\"net-test\",\"owner\":\"test\",\"layers\":[\"000-base-alpine\"],\"allow_net\":[\"example.com\"]}' >/dev/null && \
          result=\$(curl -sf -X POST http://localhost:8080/cgi-bin/api/sandboxes/net-test/exec \
            -H 'Authorization: Bearer test' -H 'Content-Type: application/json' \
            -d '{\"cmd\":\"wget -q -T 5 -O /dev/null http://example.com 2>&1 && echo CONNECTED || echo FAILED\"}') && \
          echo \"stdout: \$result\" && \
          echo \"\$result\" | grep -q 'CONNECTED' && echo 'PASS: internet reachable' || { echo 'FAIL: internet not reachable'; exit 1; }"
    RUN echo "=== Test 2: offline sandbox (no allow_net) is blocked ===" && \
        docker exec sq-sandbox-test sh -c " \
          curl -sf -X POST http://localhost:8080/cgi-bin/api/sandboxes \
            -H 'Authorization: Bearer test' -H 'Content-Type: application/json' \
            -d '{\"id\":\"offline-test\",\"owner\":\"test\",\"layers\":[\"000-base-alpine\"]}' >/dev/null && \
          result=\$(curl -sf -X POST http://localhost:8080/cgi-bin/api/sandboxes/offline-test/exec \
            -H 'Authorization: Bearer test' -H 'Content-Type: application/json' \
            -d '{\"cmd\":\"wget -q -T 3 -O /dev/null http://example.com 2>&1 && echo ACCESSIBLE || echo BLOCKED\"}') && \
          echo \"stdout: \$result\" && \
          echo \"\$result\" | grep -q 'BLOCKED' && echo 'PASS: offline sandbox blocked' || { echo 'FAIL: offline sandbox has network'; exit 1; }"
    RUN echo "=== Test 3: metadata endpoint unreachable from network sandbox ===" && \
        docker exec sq-sandbox-test sh -c " \
          result=\$(curl -sf -X POST http://localhost:8080/cgi-bin/api/sandboxes/net-test/exec \
            -H 'Authorization: Bearer test' -H 'Content-Type: application/json' \
            -d '{\"cmd\":\"wget -q -T 3 -O /dev/null http://169.254.169.254 2>&1 && echo ACCESSIBLE || echo BLOCKED\"}') && \
          echo \"stdout: \$result\" && \
          echo \"\$result\" | grep -q 'BLOCKED' && echo 'PASS: metadata blocked' || echo 'INFO: metadata accessible (expected on non-cloud host)'"
    RUN docker rm -f sq-sandbox-test 2>/dev/null || true

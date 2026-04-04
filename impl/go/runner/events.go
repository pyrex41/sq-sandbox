package runner

import (
	"encoding/json"
	"sync"
	"time"
)

// Event is a structured event emitted by the task runner.
type Event struct {
	Type      string         `json:"type"`
	Timestamp time.Time      `json:"timestamp"`
	Data      map[string]any `json:"data,omitempty"`
}

func newEvent(typ string, data map[string]any) Event {
	return Event{Type: typ, Timestamp: time.Now(), Data: data}
}

// EventLog is an append-only, subscribable event buffer.
// Subscribers receive a signal on their channel when new events are available;
// they call ReadSince to batch-read all events since their last position.
// This is the same fan-out pattern used by the sidecar's job log streaming.
type EventLog struct {
	mu     sync.Mutex
	events []Event
	subs   []*EventSub
}

type EventSub struct {
	ch chan struct{}
}

func NewEventLog() *EventLog {
	return &EventLog{}
}

// Emit appends an event and notifies all subscribers.
func (l *EventLog) Emit(typ string, data map[string]any) {
	ev := newEvent(typ, data)
	l.mu.Lock()
	l.events = append(l.events, ev)
	subs := make([]*EventSub, len(l.subs))
	copy(subs, l.subs)
	l.mu.Unlock()

	for _, s := range subs {
		select {
		case s.ch <- struct{}{}:
		default: // already signalled, subscriber will catch up
		}
	}
}

// Subscribe returns a new subscriber and a snapshot of all events so far.
func (l *EventLog) Subscribe() (*EventSub, []Event) {
	l.mu.Lock()
	defer l.mu.Unlock()
	s := &EventSub{ch: make(chan struct{}, 1)}
	l.subs = append(l.subs, s)
	snapshot := make([]Event, len(l.events))
	copy(snapshot, l.events)
	return s, snapshot
}

// Unsubscribe removes a subscriber.
func (l *EventLog) Unsubscribe(s *EventSub) {
	l.mu.Lock()
	defer l.mu.Unlock()
	for i, sub := range l.subs {
		if sub == s {
			l.subs = append(l.subs[:i], l.subs[i+1:]...)
			return
		}
	}
}

// ReadSince returns all events from position pos onward.
func (l *EventLog) ReadSince(pos int) []Event {
	l.mu.Lock()
	defer l.mu.Unlock()
	if pos >= len(l.events) {
		return nil
	}
	out := make([]Event, len(l.events)-pos)
	copy(out, l.events[pos:])
	return out
}

// Len returns the total number of events.
func (l *EventLog) Len() int {
	l.mu.Lock()
	defer l.mu.Unlock()
	return len(l.events)
}

// Notify returns the channel that signals new events.
func (s *EventSub) Notify() <-chan struct{} {
	return s.ch
}

// MarshalJSON for Event ensures clean JSON output for SSE.
func (e Event) JSON() []byte {
	b, _ := json.Marshal(e)
	return b
}

# Fiber-friendly mutex via buffered channel.
#
# ev/lock blocks the entire OS thread — wrong for net/server where
# handlers are cooperatively-scheduled fibers on one thread.
# A channel with capacity 1 gives us a fiber-yielding mutex:
# ev/take suspends only the calling fiber, not the whole thread.

(defn make-lock []
  "Create a fiber-friendly mutex."
  (def ch (ev/chan 1))
  (ev/give ch :free)
  ch)

(defmacro with-lock [l & body]
  "Acquire lock, run body, release on completion or error."
  ~(do
    (ev/take ,l)
    (defer (ev/give ,l :free)
      ,;body)))

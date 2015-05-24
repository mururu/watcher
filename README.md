# watcher

### Example

```
1> application:start(watcher).
ok
2> A = spawn(fun() -> watcher:run([message_queue_len], #{name => "p1"}), timer:sleep(20000) end).
<0.39.0>
3> "p1":
  message_queue_len: 0
"p1":
  message_queue_len: 0
3> A ! a.
a
4> "p1":
  message_queue_len: 1
"p1":
  message_queue_len: 1
"p1":
  message_queue_len: 1

...
```

Adapter function to convert a [`Network.Wai.Application`][Application] to a [Snap][] handler.

```haskell
serveWai :: MonadSnap m => Application -> m ()
```

It works for few a very basic test cases, but hasn't been tested much
on a real app yet.

[Application]: https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:Application
[Snap]: https://hackage.haskell.org/package/snap-core-1.0.4.0/docs/Snap-Core.html#t:Snap

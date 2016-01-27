0.1.2
---

* Fixes:
    * Fix default spec for `ReqBody` param to be required (see [#22](https://github.com/haskell-servant/servant-swagger/issues/22));
    * Set version bounds for `swagger2`.

0.1.1
---

* Fixes:
    * Fix `subOperations` to filter endpoints also by method (see [#18](https://github.com/haskell-servant/servant-swagger/issues/18));
    * Fix response schema in `ToSwagger` instance for `Header` (see [b59e557](https://github.com/haskell-servant/servant-swagger/commit/b59e557a05bc2669332c52b397879e7598747b82)).

0.1
---
* Major changes
    * Use `swagger2` for data model (see [#9](https://github.com/dmjio/servant-swagger/pull/9)); this changes almost everything.

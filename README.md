`bun-wasm`
====================

Export Haskell functions to WASM modules for use with the [bun](https://bun.com/) test runner.

```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "add" add :: Int -> Int -> Int
foreign export javascript "sub" sub :: Int -> Int -> Int
#endif
-----------------------------------------------------------------------------
add :: Int -> Int -> Int
add = (+)
-----------------------------------------------------------------------------
sub :: Int -> Int -> Int
sub = (-)
-----------------------------------------------------------------------------
main :: IO ()
main = pure ()
-----------------------------------------------------------------------------
```

```typescript
/* haskell wasm exports */
const { add, sub } = await instance.exports;

/* tests */
describe('Should test arithmetic', () => {
  test('222 + 222 should equal 444', () => {
      add(222,222).then ((result) => {
        expect(result).toEqual(444);
      });
  });
  test('2 - 8 should equal -6', () => {
      sub(2,8).then ((result) => {
        expect(result).toEqual(-6);
      });
  });
});
```

### Build and test

```bash
$ nix develop .#wasm --command bash -c 'make' && cabal clean && bun test
```

### Result

```
bun test v1.2.18 (0d4089ea)

index.spec.ts:
✓ Should test arithmetic > 222 + 222 should equal 444 [3.00ms]
✓ Should test arithmetic > 2 - 8 should equal -6

 2 pass
 0 fail
 2 expect() calls
Ran 2 tests across 1 file. [240.00ms]
```

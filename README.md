:rice: `bun-wasm`
====================

Export Haskell functions to WASM modules for use with the [bun](https://bun.com/) test runner.

```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultilineStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "add" add :: Int -> Int -> Int
foreign export javascript "sub" sub :: Int -> Int -> Int
foreign export javascript "domAppend" domAppend :: IO ()
#endif
-----------------------------------------------------------------------------
add :: Int -> Int -> Int
add x y = x + y
-----------------------------------------------------------------------------
sub :: Int -> Int -> Int
sub x y = x - y
-----------------------------------------------------------------------------
main :: IO ()
main = pure ()
-----------------------------------------------------------------------------
foreign import javascript
  """
  document.body.appendChild(document.createElement('div'));
  """ domAppend :: IO ()
-----------------------------------------------------------------------------
```

```typescript
/* haskell wasm exports */
const { add, sub, domAppend } = await instance.exports;

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

describe('Should perform DOM operations', () => {
  test('DOM append test', () => {
      expect(document.body.childNodes.length).toEqual(0);
      domAppend().then (() => {
        expect(document.body.childNodes.length).toEqual(1);
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
bun test v1.2.20 (6ad208bc)

index.spec.ts:
✓ Should test arithmetic > 222 + 222 should equal 444 [3.00ms]
✓ Should test arithmetic > 2 - 8 should equal -6
✓ Should perform DOM operations > DOM append test [1.00ms]

 3 pass
 0 fail
 4 expect() calls
Ran 3 tests across 1 file. [198.00ms]
```

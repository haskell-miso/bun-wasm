/* imports */
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import ghc_wasm_jsffi from "./public/ghc_wasm_jsffi.js";
import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";

/* options */
const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};

const wasmBuffer = await Bun.file("./public/app.wasm").arrayBuffer();
const { instance } = await WebAssembly.instantiate(wasmBuffer, {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});

Object.assign(instance_exports, instance.exports);
wasi.initialize(instance);

/* silence */
beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
});

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
});

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


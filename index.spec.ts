/* imports */
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { instance } from './app.ts';

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


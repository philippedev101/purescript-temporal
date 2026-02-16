import { Temporal } from "@js-temporal/polyfill";
globalThis.Temporal = Temporal;

const main = await import("./output/Test.Main/index.js");
main.main();

// `noon` is the 12:00:00 PlainTime. The global `Temporal` is installed by the
// polyfill self-install in Internal/Types.js, which loads first because this
// module imports Temporal.Internal.Types.
export const noon = new Temporal.PlainTime(12, 0, 0, 0, 0, 0);

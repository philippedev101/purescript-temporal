export const fromEpochNanosecondsImpl = (just) => (nothing) => (ns) => {
  try {
    return just(new Temporal.Instant(ns));
  } catch (_) {
    return nothing;
  }
};

export const fromEpochMillisecondsImpl = (just) => (nothing) => (ms) => {
  try {
    return just(Temporal.Instant.fromEpochMilliseconds(ms));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.Instant.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getEpochMilliseconds = (i) => i.epochMilliseconds;
export const getEpochNanoseconds = (i) => i.epochNanoseconds;

export const addImpl = (just) => (nothing) => (dur) => (inst) => {
  try {
    return just(inst.add(dur));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (dur) => (inst) => {
  try {
    return just(inst.subtract(dur));
  } catch (_) {
    return nothing;
  }
};

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);
export const roundImpl = (opts) => (inst) => inst.round(opts);

export const toZonedDateTimeISO = (tz) => (inst) => inst.toZonedDateTimeISO(tz);

export const toString = (i) => i.toString();

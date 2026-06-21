export const durationImpl = (just) => (nothing) => (fields) => {
  try {
    return just(new Temporal.Duration(
      fields.years, fields.months, fields.weeks, fields.days,
      fields.hours, fields.minutes, fields.seconds,
      fields.milliseconds, fields.microseconds, fields.nanoseconds
    ));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.Duration.from(str));
  } catch (_) {
    return nothing;
  }
};

export const unsafeDuration = (fields) =>
  new Temporal.Duration(
    fields.years, fields.months, fields.weeks, fields.days,
    fields.hours, fields.minutes, fields.seconds,
    fields.milliseconds, fields.microseconds, fields.nanoseconds
  );

export const getYears = (d) => d.years;
export const getMonths = (d) => d.months;
export const getWeeks = (d) => d.weeks;
export const getDays = (d) => d.days;
export const getHours = (d) => d.hours;
export const getMinutes = (d) => d.minutes;
export const getSeconds = (d) => d.seconds;
export const getMilliseconds = (d) => d.milliseconds;
export const getMicroseconds = (d) => d.microseconds;
export const getNanoseconds = (d) => d.nanoseconds;
export const sign = (d) => d.sign;
export const blank = (d) => d.blank;

export const addImpl = (just) => (nothing) => (relativeTo) => (a) => (b) => {
  try {
    const opts = relativeTo === null ? undefined : { relativeTo };
    return just(a.add(b, opts));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (relativeTo) => (a) => (b) => {
  try {
    const opts = relativeTo === null ? undefined : { relativeTo };
    return just(a.subtract(b, opts));
  } catch (_) {
    return nothing;
  }
};

export const negated = (d) => d.negated();
export const abs = (d) => d.abs();

export const roundImpl = (just) => (nothing) => (opts) => (relativeTo) => (d) => {
  try {
    const roundOpts = { ...opts };
    if (relativeTo !== null) roundOpts.relativeTo = relativeTo;
    return just(d.round(roundOpts));
  } catch (_) {
    return nothing;
  }
};

export const totalImpl = (just) => (nothing) => (unit) => (relativeTo) => (d) => {
  try {
    const opts = relativeTo === null ? unit : { unit, relativeTo };
    return just(d.total(opts));
  } catch (_) {
    return nothing;
  }
};

export const compareImpl = (relativeTo) => (a) => (b) =>
  Temporal.Duration.compare(a, b, { relativeTo });

export const toString = (d) => d.toString();

// RelativeTo helpers — identity at runtime, exist for PureScript type safety
export const noRelativeToJS = null;
export const relDateToJS = (d) => d;
export const relDateTimeToJS = (dt) => dt;
export const relZonedToJS = (z) => z;

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

export const addImpl = (just) => (nothing) => (a) => (b) => {
  try {
    return just(a.add(b));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (a) => (b) => {
  try {
    return just(a.subtract(b));
  } catch (_) {
    return nothing;
  }
};

export const negated = (d) => d.negated();
export const abs = (d) => d.abs();

export const roundImpl = (just) => (nothing) => (opts) => (d) => {
  try {
    return just(d.round(opts));
  } catch (_) {
    return nothing;
  }
};

export const totalImpl = (just) => (nothing) => (unit) => (d) => {
  try {
    return just(d.total(unit));
  } catch (_) {
    return nothing;
  }
};

export const toString = (d) => d.toString();

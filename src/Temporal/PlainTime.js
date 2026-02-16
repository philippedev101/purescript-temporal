export const plainTimeImpl = (just) => (nothing) => (fields) => {
  try {
    return just(new Temporal.PlainTime(
      fields.hour, fields.minute, fields.second,
      fields.millisecond, fields.microsecond, fields.nanosecond
    ));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.PlainTime.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getHour = (t) => t.hour;
export const getMinute = (t) => t.minute;
export const getSecond = (t) => t.second;
export const getMillisecond = (t) => t.millisecond;
export const getMicrosecond = (t) => t.microsecond;
export const getNanosecond = (t) => t.nanosecond;

export const withImpl = (just) => (nothing) => (fields) => (pt) => {
  try {
    return just(pt.with(fields));
  } catch (_) {
    return nothing;
  }
};

export const add = (dur) => (pt) => pt.add(dur);
export const subtract = (dur) => (pt) => pt.subtract(dur);

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);
export const roundImpl = (opts) => (pt) => pt.round(opts);

export const toPlainDateTime = (date) => (time) => date.toPlainDateTime(time);

export const toString = (t) => t.toString();

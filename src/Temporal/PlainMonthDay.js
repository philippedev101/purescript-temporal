export const plainMonthDayImpl = (just) => (nothing) => (m) => (d) => {
  try {
    return just(new Temporal.PlainMonthDay(m, d));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.PlainMonthDay.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getMonthCode = (md) => md.monthCode;
export const getDay = (md) => md.day;

export const withImpl = (just) => (nothing) => (fields) => (pmd) => {
  try {
    return just(pmd.with(fields));
  } catch (_) {
    return nothing;
  }
};

export const toPlainDateImpl = (just) => (nothing) => (year) => (pmd) => {
  try {
    return just(pmd.toPlainDate({ year }));
  } catch (_) {
    return nothing;
  }
};

export const toString = (md) => md.toString();

export const fastForeachThunk = (as) => {
  for (var i = 0, l = as.length; i < l; i++) {
    as[i]();
  }
};

export const fastForeachE = (as, f) => {
  for (var i = 0, l = as.length; i < l; i++) {
    f(as[i]);
  }
};

export const fastForeachST = (as, f) => {
  for (var i = 0, l = as.length; i < l; i++) {
    f(as[i]);
  }
};

export const fastForeachOhE = (o, f) => {
  o.forEach((v) => {
    f(v);
  });
};

export const addToMapForeign = (k) => (v) => (m) => () => {
  m.set(k, v);
};
export const deleteFromMapForeign = (k) => (m) => () => {
  return m.delete(k);
};
export const newForeignMap = () => {
  return new Map();
};

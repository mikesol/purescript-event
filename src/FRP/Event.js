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
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////

export const objHack = () => {
  return { r: false, q: [], m: [{}] };
};

export const insertObjHack = (k, v, o) => {
  o.m[o.m.length - 1][k] = v;
};

export const deleteObjHack = (k, o) => {
  for (const m of o.m) {
    if (delete m[k]) {
      return true;
    }
  }
  return false;
};

export const fastForeachOhE = (o, f) => {
  if (o.r) {
    o.q.push(() => {fastForeachOhE(o, f)});
    return;
  }
  o.r = true;
  const M = {};
  const run = (i) => {
    o.m.push({});
    for (const kv of Object.entries(o.m[i]))  {
      const k = kv[0];
      const v = kv[1];
      f(v);
      if (Object.keys(o.m[i + 1]).length) run(i + 1);
      o.m[i + 1] = {};
      o.m.length = i + 1 + 1;
      M[k] = v;
    };
  };
  run(0);
  o.m.length = 0;
  o.m.push(M);
  let fn;
  o.r = false;
  while (fn = o.q.shift()) {
    fn();
  }
};

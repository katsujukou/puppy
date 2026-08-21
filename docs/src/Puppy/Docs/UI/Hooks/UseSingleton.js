export const _mkSingleton = (id_, val) => (() => {
  const id = Symbol.for(id_);
  if (globalThis[id]) {
    throw new Error(`The id ${id_} is already in use.`);
  }
  globalThis[id] = val;
  return () => {
    return id;
  };
})();

/**
 * 
 * @param {Symbol} id 
 */
export const _getSingleton = (id) => {
  if (!globalThis[id]) {
    throw new Error(`No such a singleton instance: ${id.description}`);
  }
  return globalThis[id];
}

export const _modifySingleton = (f, id) => {
  if (!globalThis[id]) {
    throw new Error(`No such a singleton instance: ${id.description}`);
  }
  globalThis[id] = f(globalThis[id]);
  return globalThis[id];
}
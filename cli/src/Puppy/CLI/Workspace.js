export const parseJsonImpl = (onError) => (onOk) => (text) => {
  try {
    return onOk(JSON.parse(text));
  } catch (e) {
    return onError(e.message);
  }
};

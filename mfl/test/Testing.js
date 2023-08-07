export const try_ = makeErr => makeOk => m => () => {
  try {
    const x = m();
    return makeOk(x);
  } catch (error) {
    return makeErr(error);
  }
}
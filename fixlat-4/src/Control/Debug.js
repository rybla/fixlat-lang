export const _debug = x => k => {
  console.log(x)
  return k(x)
}
export const unsafeBackdoor = (mev) => (backdoor) => () => {
  const oldMakeEvent = backdoor.makeEvent;
  backdoor.makeEvent = mev;
  return oldMakeEvent;
}
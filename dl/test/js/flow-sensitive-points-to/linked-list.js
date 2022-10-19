(function llFindTruthy(l) {
  if (l == null) return l;
  while (l !== null) {
    if (l.data) return l;
    l = l.next;
  }
})();

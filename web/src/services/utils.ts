export const parseQueryString = () => {
  if (!document.location.search) return;
  const querys = document.location.search.split('?')[1].split('&');
  let params = {};
  querys.forEach(q => {
    const qs = q.split('=');
    const key = qs[0];
    const value = qs[1];
    if (params[key]) {
      if (!(params[key] instanceof Array)) {
        params[key] = [params[key]];
      }
      if (params[key].indexOf(value) === -1) {
        params[key].push(value);
      }
    } else {
      params[key] = value;
    }
  });
  return params;
}

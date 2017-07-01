const get = (url, isJson) => {
  return new Promise((resolve, reject) => {
    let request = new XMLHttpRequest();
    request.open('GET', url);

    request.addEventListener('load', function() {
      if (this.status >= 200 && this.status < 400) {
        resolve(isJson ? JSON.parse(this.responseText) : this.responseText);
      } else {
        reject(this.responseText);
      }
    });
    request.send();
  });
}

class Http {
  get(url) {
    return get(url, false);
  }

  getJson(url) {
    return get(url, true);
  }
}

export default Http;

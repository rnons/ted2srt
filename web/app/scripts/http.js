function get(url) {
  return new Promise((resolve, reject) => {
    let request = new XMLHttpRequest();
    request.open('GET', url);

    request.onload = function() {
      if (this.status >= 200 && this.status < 400) {
        resolve(JSON.parse(this.responseText));
      } else {
        reject(this.responseText);
      }
    };
    request.send();
  })
}

export default {
  get
}

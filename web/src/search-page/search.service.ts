import Talk from '../models/talk';
import Http from '../services/http';


class SearchService {
  talks: Talk[];
  q: string;

  constructor(private http: Http) {}

  search(q) {
    this.q = q;
    return this.http.getJson(`/api/search?q=${q}`).then((data: any[]) => {
      this.talks = data.map(d => new Talk(d));
      return Promise.resolve();
    }).catch(err => {
      console.log(err);
    });
  }
}

export default SearchService;

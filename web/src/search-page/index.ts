import Http from '../services/http';
import SearchService from './search.service';
import SearchComponent from './search.component';


interface Query {
  q?: string;
}

class SearchPage {
  service: SearchService;
  searchPage: SearchComponent;

  constructor(http: Http, private root: HTMLElement, query: Query) {
    document.title = query.q + '- TED2srt Search';
    this.service = new SearchService(http);
    this.service.search(query.q).then(() => {
      this.searchPage = new SearchComponent(this.service);
      this.render();
    })
  }

  render = () => {
    this.root.innerHTML = this.searchPage.render();
  }
}

export default SearchPage;

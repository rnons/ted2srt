import Http from '../services/http';
import Footer from '../components/footer';
import SearchService from './search.service';
import SearchComponent from './search.component';


interface Query {
  q?: string;
}

class SearchPage {
  service: SearchService;
  footer: Footer;
  searchPage: SearchComponent;

  constructor(http: Http, private root: HTMLElement, query: Query) {
    this.service = new SearchService(http);
    this.service.search(query.q).then(() => {
      this.searchPage = new SearchComponent(this.service, this.footer);
      this.render();
    })
    document.title = query.q + '- TED2srt Search';
    this.footer = new Footer(http);
  }

  render = () => {
    this.root.innerHTML = this.searchPage.render();
    this.searchPage.mounted();
  }
}

export default SearchPage;

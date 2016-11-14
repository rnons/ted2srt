import Http from '../services/http';
import Footer from '../components/footer';
import HomeService from './home.service';
import HomeComponent from './home.component';


class Home {
  footer: Footer;
  homePage: HomeComponent;

  constructor(http: Http, private root: HTMLElement) {
    const service = new HomeService(http);
    service.fetch().then(() => {
      this.homePage = new HomeComponent(service, this.footer);
      this.render();
    });
    document.title = 'TED2srt: Download bilingual subtitles of TED talks';
    this.footer = new Footer(http);
  }

  render() {
    this.root.innerHTML = this.homePage.render();
    this.homePage.mounted();
  }
}

export default Home;

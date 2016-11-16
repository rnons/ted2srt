import Http from '../services/http';
import Footer from '../components/footer';
import TalkService from './talk.service';
import TalkComponent from './talk.component';


class TalkPage {
  service: TalkService;
  footer: Footer;
  talkPage: TalkComponent;

  constructor(http: Http, private root: HTMLElement, slug: string) {
    this.service = new TalkService(http);
    this.service.getBySlug(slug).then(() => {
      document.title = this.service.talk.name + '- TED2srt';
      this.talkPage = new TalkComponent(this.service, this.footer);
      this.render();
      this.service.subscribe(() => this.talkPage.rerender());
    })
    this.footer = new Footer(http);
  }

  render() {
    this.root.innerHTML = this.talkPage.render();
    this.talkPage.mounted();
  }
}

export default TalkPage;

import Http from '../services/http';
import TalkService from './talk.service';
import TalkComponent from './talk.component';


class TalkPage {
  service: TalkService;
  talkPage: TalkComponent;

  constructor(http: Http, private root: HTMLElement, slug: string) {
    this.service = new TalkService(http);
    this.service.getBySlug(slug).then(() => {
      this.talkPage = new TalkComponent(this.service);
      this.render();
      this.service.subscribe(this.render);
    })
  }

  render = () => {
    this.root.innerHTML = this.talkPage.render();
    this.talkPage.mounted();
  }
}

export default TalkPage;

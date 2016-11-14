import Header from '../components/header';
import Footer from '../components/footer';
import SearchService from './search.service';
import * as styles from './search.css';


class SearchComponent {
  header = new Header();
  footer = new Footer();

  constructor(private service: SearchService) {}

  renderTalk(talk) {
    const {
      slug,
      name,
      image,
      description,
      publishedAt
    } = talk;
    return `
      <div class="${styles.item}">
        <h3><a href="/#/talks/${slug}">${name}</a></h3>
        <div class="${styles.info}">
          <a class="${styles.cover}"
            href="/#/talks/${slug}"
            style="background-image: url(${image})">
          </a>
          <div>
            <p>${description}</p>
            <p class="${styles.date}">Published: ${publishedAt.toDateString()}</p>
          </div>
        </div>
      </div>
    `
  }
  render() {
    const header = this.header.render();
    const footer = this.footer.render();
    const talks = this.service.talks.map(this.renderTalk).join('');
    return `
      ${header}
      <div class="${styles.root}">
        ${talks}
      </div>
      ${footer}
    `;
  }
}

export default SearchComponent;

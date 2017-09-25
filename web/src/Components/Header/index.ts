import SearchForm from '../search-form';
import * as styles from './index.css';


class Header {
  constructor(private q: string = '') {}

  searchForm = new SearchForm(this.q);

  render() {
    const form = this.searchForm.render();
    return `
      <div class="${styles.header}">
        <div class="${styles.container}">
          <a class="${styles.logo}"
             href="/#/">:: TED -> [SRT]</a>
          ${form}
        </div>
      </div>
    `;
  }
}

export default Header;

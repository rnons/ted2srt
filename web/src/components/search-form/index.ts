import * as styles from './index.css';


class SearchForm {
  constructor(private q: string = '') {}

  render() {
    return `
      <form class="${styles.form}"
            method="GET"
            action="#/search">
        <input type="text"
               class="${styles.input}"
               name="q"
               value="${this.q}"
               placeholder="TED talk url or keywords"
               required>
        <input type="submit" hidden>
      </form>
    `;
  }
}

export default SearchForm;

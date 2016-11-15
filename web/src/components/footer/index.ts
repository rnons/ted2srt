import Http from '../../services/http';
import * as styles from './index.css';


class Footer {
  constructor(private http: Http) {}

  mounted() {
    document.querySelector('.js-random').addEventListener('click', () => {
      return this.http.get('/api/talks/random').then((data: { slug: string }) => {
        document.location.href = '/#/talks/' + data.slug;
      }).catch(err => {
        console.log(err);
      });
    });
  }

  render() {
    return `
      <footer class="${styles.footer}">
        TED2srt by <a href="https://twitter.com/rnons" target="_blank">rnons</a> |
        <a class="js-random">random talk</a> |
        <a href="https://github.com/rnons/ted2srt" target="_blank">source code</a> |
        <a href="/atom.xml" target="_blank">feed</a>
      </footer>
    `;
  }
}

export default Footer;

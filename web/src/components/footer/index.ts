import * as styles from './index.css';


class Footer {
  render() {
    return `
      <footer class="${styles.footer}">
        TED2srt by <a href="https://twitter.com/rnons" target="_blank">rnons</a> |
        <a id="random-talk">random talk</a> |
        <a href="https://github.com/rnons/ted2srt" target="_blank">source code</a> |
        <a href="http://ted2srt.org/atom.xml" target="_blank">feed</a>
      </footer>
    `;
  }
}

export default Footer;

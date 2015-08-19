var assert = require('assert'),
    webdriver = require('selenium-webdriver'),
    test = require('selenium-webdriver/testing'),
    By = webdriver.By,
    until = webdriver.until;

var driver;

test.describe('Talk page', () => {
  test.before(() => {
    driver = new webdriver.Builder()
      .forBrowser('firefox')
      .build();
  });

  test.beforeEach(() => {
    driver.get('http://localhost:9001/#/talks/randall_munroe_comics_that_ask_what_if');
    driver.navigate().refresh();
  });

  test.afterEach(() => {
    driver.executeScript('window.localStorage.clear()');
  });

  test.after(() => {
    driver.quit();
  });

  test.it('should have many languages', () => {
    driver.wait(until.elementsLocated(By.css('#languages li')), 1000);
    driver.findElements(By.css('#languages li'))
      .then((eles) => {
        assert(eles.length > 20);
      });
  });

  test.it('should be able to select a language', () => {
    driver.wait(until.elementsLocated(By.css('li[data-code="en"')), 1000);
    driver.findElement(By.css('li[data-code="en"')).click();
    driver.findElements(By.css('#languages li.selected'))
      .then((eles) => {
        assert.equal(eles.length, 1);
      });
  });

  test.it('should be able to deselect a language', () => {
    driver.wait(until.elementsLocated(By.css('li[data-code="en"')), 1000);
    var item = driver.findElement(By.css('li[data-code="en"'));
    item.click();
    item.click();
    driver.findElements(By.css('#languages li.selected'))
      .then((eles) => {
        assert.equal(eles.length, 0);
      });
  });

  test.it('should not be able to select more than two languages', () => {
    var item;
    driver.wait(until.elementsLocated(By.css('li[data-code="en"')), 1000);
    driver.findElement(By.css('li[data-code="en"')).click()
    driver.findElement(By.css('li[data-code="zh-cn"')).click();
    item = driver.findElement(By.css('li[data-code="zh-tw"'));
    item.click();
    driver.findElements(By.css('#languages li.selected'))
      .then((eles) => {
        assert.equal(eles.length, 2);
      });
    item.getAttribute('class')
      .then((classes) => {
        assert.equal(classes.indexOf('selected'), -1);
      });
  });

  test.it('should store/read selected to/from localStorage', () => {
    driver.wait(until.elementsLocated(By.css('li[data-code="en"')), 1000);
    driver.findElement(By.css('li[data-code="en"')).click()
    driver.navigate().refresh();
    driver.findElements(By.css('#languages li.selected'))
      .then((eles) => {
        assert.equal(eles.length, 1);
      });
    driver.findElement(By.css('li[data-code="en"')).getAttribute('class')
      .then((classes) => {
        assert.notEqual(classes.indexOf('selected'), -1);
      });
  });
});

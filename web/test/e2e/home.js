var assert = require('assert'),
    webdriver = require('selenium-webdriver'),
    test = require('selenium-webdriver/testing'),
    By = webdriver.By,
    until = webdriver.until;

var driver;

test.describe('homepage', () => {
  test.before(() => {
    driver = new webdriver.Builder()
      .forBrowser('firefox')
      .build();
  });

  test.beforeEach(() => {
    driver.get('http://localhost:9001');
  });

  test.after(() => {
    driver.quit();
  });

  test.it('should show five talks', () => {
    driver.wait(until.elementsLocated(By.className('tile')), 1000);
    driver.findElements(By.className('tile'))
      .then((eles) => {
        assert.equal(eles.length, 5);
      });
  });

  test.it('should redirect to search page', () => {
    var input, query, expected;
    query = 'google';
    expected = 'http://localhost:9001/?q=google#/search';
    driver.wait(until.elementsLocated(By.name('q')), 1000);
    input = driver.findElement(By.name('q'));
    input.sendKeys(query);
    input.sendKeys(webdriver.Key.ENTER);
    driver.getCurrentUrl().then((url) => {
      assert.equal(url, expected);
    });
  });

  test.it('should redirect to talk page', () => {
    var input, query, expected;
    query = 'https://www.ted.com/talks/randall_munroe_comics_that_ask_what_if';
    expected = 'http://localhost:9001/#/talks/randall_munroe_comics_that_ask_what_if';
    driver.wait(until.elementsLocated(By.name('q')), 1000);
    input = driver.findElement(By.name('q'));
    input.sendKeys(query);
    input.sendKeys(webdriver.Key.ENTER);
    driver.getCurrentUrl().then((url) => {
      assert.equal(url, expected);
    });
  });
});

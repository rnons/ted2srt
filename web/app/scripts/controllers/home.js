export class HomeController {
  constructor(Talks, View) {
    this.Talks = Talks;
    this.View = View;

    this.Talks.fetch().then((talks) => {
      this.View.render(talks);
    });
  }
};

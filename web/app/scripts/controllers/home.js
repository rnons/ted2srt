export class HomeController {
  constructor(talks, view) {
    this.talks = talks;
    this.view = view;

    this.view.render(talks);
  }
}

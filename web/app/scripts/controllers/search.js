export class SearchController {
  constructor(talks, view, query) {
    this.talks = talks;
    this.view = view;

    this.view.render(this.talks, query);
  }
}

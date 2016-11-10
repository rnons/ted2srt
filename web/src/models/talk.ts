/**
 * Destructuring with type annotation is not great in typescript.
 * See https://github.com/Microsoft/TypeScript/issues/5034
 */

class Talk {
  id: number;
  name: string;
  speaker: string;
  title: string;
  description: string;
  slug: string;
  mediaSlug: string;
  publishedAt: Date;
  image: string;
  languages: any[];
  hasAudio: boolean;

  constructor(params) {
    const {
      id,
      name,
      description,
      slug,
      mediaSlug,
      publishedAt,
      image,
      languages,
      hasAudio
    } = params;

    this.id = id;
    this.name = name;
    [this.speaker, this.title] = this.name.split(':').map(s => s.trim());
    this.description = description;
    this.slug = slug;
    this.mediaSlug = mediaSlug;
    this.publishedAt = new Date(publishedAt);
    this.image = image;
    this.languages = languages;
    this.hasAudio = hasAudio;
  }
}

export default Talk;

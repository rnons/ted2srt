--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.2
-- Dumped by pg_dump version 9.5.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = public, pg_catalog;

--
-- Data for Name: talks; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO talks (id, name, slug, filmed, published, description, image, languages, media_slug, media_pad) VALUES (2482, 'Joshua Prager: Wisdom from great writers on every year of life', 'joshua_prager_wisdom_from_great_writers_on_every_year_of_life', '2015-03-19 08:00:00+08', '2016-04-19 21:08:58+08', 'As different as we humans are from one another, we all age along the same great sequence, and the shared patterns of our lives pass into the pages of the books we love. In this moving talk, journalist Joshua Prager explores the stages of life through quotations from Norman Mailer, Joyce Carol Oates, William Trevor and other great writers, set to visualizations by graphic designer Milton Glaser. &quot;Books tell us who we&#39;ve been, who we are, who we will be, too,&quot; Prager says.', 'https://tedcdnpi-a.akamaihd.net/r/tedcdnpe-a.akamaihd.net/images/ted/57e977ee55231074e78e4b4d0cb0d1812f86a8d9_2880x1620.jpg?c=1050%2C550&amp;w=1050', '[{"languageCode": "en", "languageName": "English"}, {"languageCode": "fa", "languageName": "Persian"}, {"languageCode": "es", "languageName": "Spanish"}]', 'JoshPrager_2015A', 11820);
INSERT INTO talks (id, name, slug, filmed, published, description, image, languages, media_slug, media_pad) VALUES (2485, 'Chris Anderson: TED''s secret to great public speaking', 'chris_anderson_teds_secret_to_great_public_speaking', '2016-03-24 08:00:00+08', '2016-04-19 13:00:00+08', 'There&#39;s no single formula for a great talk, but there is a secret ingredient that all the best ones have in common. TED Curator Chris Anderson shares this secret -- along with four ways to make it work for you. Do you have what it takes to share an idea worth spreading?', 'https://tedcdnpi-a.akamaihd.net/r/tedcdnpe-a.akamaihd.net/images/ted/69d74807799b754a4ce42394fb5b2134766f27f2_2880x1620.jpg?c=1050%2C550&amp;w=1050', '[{"languageCode": "en", "languageName": "English"}, {"languageCode": "fa", "languageName": "Persian"}, {"languageCode": "pt-br", "languageName": "Portuguese, Brazilian"}]', 'ChrisAnderson_GreatTEDTalk_2016V', 11820);
INSERT INTO talks (id, name, slug, filmed, published, description, image, languages, media_slug, media_pad) VALUES (2480, 'Christiana Figueres: The inside story of the Paris climate agreement', 'christiana_figueres_the_inside_story_of_the_paris_climate_agreement', '2016-02-17 08:00:00+08', '2016-04-18 22:55:20+08', 'What would you do if your job was to save the planet? When Christiana Figueres was tapped by the UN to lead the Paris climate conference (COP 21) in December 2015, she reacted the way many people would: she thought it would be impossible to bring the leaders of 195 countries into agreement on how to slow climate change. Find out how she turned her skepticism into optimism -- and helped the world achieve the most important climate agreement in history.', 'https://tedcdnpi-a.akamaihd.net/r/tedcdnpe-a.akamaihd.net/images/ted/57a8bbe6d97cb1d80fa225c3bce02b2d59c8d817_2880x1620.jpg?c=1050%2C550&amp;w=1050', '[{"languageCode": "en", "languageName": "English"}]', 'ChristianaFigueres_2016', 11820);
INSERT INTO talks (id, name, slug, filmed, published, description, image, languages, media_slug, media_pad) VALUES (2483, 'Aditi Gupta: A taboo-free way to talk about periods', 'aditi_gupta_a_taboo_free_way_to_talk_about_periods', '2015-05-29 08:00:00+08', '2016-04-21 23:00:16+08', 'It&#39;s true: talking about menstruation makes many people uncomfortable. And that taboo has consequences: in India, three out of every 10 girls don&#39;t even know what menstruation is at the time of their first period, and restrictive customs related to periods inflict psychological damage on young girls. Growing up with this taboo herself, Aditi Gupta knew she wanted to help girls, parents and teachers talk about periods comfortably and without shame. She shares how she did it.', 'https://tedcdnpi-a.akamaihd.net/r/tedcdnpe-a.akamaihd.net/images/ted/1c84d70a3119a6650492a1531cf45fcde08f08fc_2880x1620.jpg?c=1050%2C550&amp;w=1050', '[{"languageCode": "en", "languageName": "English"}, {"languageCode": "sr", "languageName": "Serbian"}, {"languageCode": "tr", "languageName": "Turkish"}]', 'AditiGupta_2015X', 11820);
INSERT INTO talks (id, name, slug, filmed, published, description, image, languages, media_slug, media_pad) VALUES (2473, 'Juan Enriquez: We can reprogram life. How to do it wisely', 'juan_enriquez_we_can_reprogram_life_how_to_do_it_wisely', '2015-11-06 08:00:00+08', '2016-04-20 23:04:46+08', 'For four billion years, what lived and died on Earth depended on two principles: natural selection and random mutation. Then humans came along and changed everything — hybridizing plants, breeding animals, altering the environment and even purposefully evolving ourselves. Juan Enriquez provides five guidelines for a future where this ability to program life rapidly accelerates. &quot;This is the single most exciting adventure human beings have been on,&quot; Enriquez says. &quot;This is the single greatest superpower humans have ever had.&quot;', 'https://tedcdnpi-a.akamaihd.net/r/tedcdnpe-a.akamaihd.net/images/ted/0dfafd749af481f0d95ad4af910ebecaff4d26fa_2880x1620.jpg?c=1050%2C550&amp;w=1050', '[{"languageCode": "en", "languageName": "English"}, {"languageCode": "ru", "languageName": "Russian"}]', 'JuanEnriquez_2015P', 11820);


--
-- Data for Name: transcripts; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO transcripts (id, name, en) VALUES (2473, 'Juan Enriquez: We can reprogram life. How to do it wisely', '﻿
So, there''s an actor called Dustin Hoffman. And years ago, he made this movie which some of you may have heard of, called "The Graduate." And there''s two key scenes in that movie. The first one is the seduction scene. I''m not going to talk about that tonight. 

(Laughter) 

The second scene is where he''s taken out by the old guy to the pool, and as a young college graduate, the old guy basically says one word, just one word. And of course, all of you know what that word is. It''s "plastics." 

(Laughter) 

And the only problem with that is, it was completely the wrong advice. 

(Laughter) 

Let me tell you why it was so wrong. The word should have been "silicon." And the reason it should have been silicon is because the basic patents for semiconductors had already been made, had already been filed, and they were already building them. So Silicon Valley was just being built in 1967, when this movie was released. And the year after the movie was released, Intel was founded. So had the graduate heard the right one word, maybe he would have ended up onstage -- oh, I don''t know -- maybe with these two. 

(Laughter) 

So as you''re thinking of that, let''s see what bit of advice we might want to give so that your next graduate doesn''t become a Tupperware salesman. 

(Laughter) 

So in 2015, what word of advice would you give people, when you took a college graduate out by the pool and you said one word, just one word? I think the answer would be "lifecode." So what is "lifecode?" Lifecode is the various ways we have of programming life. So instead of programming computers, we''re using things to program viruses or retroviruses or proteins or DNA or RNA or plants or animals, or a whole series of creatures. And as you''re thinking about this incredible ability to make life do what you want it to do, what it''s programmed to do, what you end up doing is taking what we''ve been doing for thousands of years, which is breeding, changing, mixing, matching all kinds of life-forms, and we accelerate it. 

And this is not something new. This humble mustard weed has been modified so that if you change it in one way, you get broccoli. And if you change it in a second way, you get kale. And if you change it in a third way, you get cauliflower. So when you go to these all-natural, organic markets, you''re really going to a place where people have been changing the lifecode of plants for a long time. The difference today, to pick a completely politically neutral term -- 

[Intelligent design] 

(Laughter) 

We''re beginning to practice intelligent design. That means that instead of doing this at random and seeing what happens over generations, we''re inserting specific genes, we''re inserting specific proteins, and we''re changing lifecode for very deliberate purposes. And that allows us to accelerate how this stuff happens. 

Let me just give you one example. Some of you occasionally might think about sex. And we kind of take it for granted how we''ve changed sex. So we think it''s perfectly normal and natural to change it. What''s happened with sex over time is -- normally, sex equals baby, eventually. But in today''s world, sex plus pill equals no baby. 

(Laughter) 

And again, we think that''s perfectly normal and natural, but that has not been the case for most of human history. And it''s not the case for animals. What it is does is it gives us control, so sex becomes separate from conception. And as you''re thinking of the consequences of that, then we''ve been playing with stuff that''s a little bit more advanced, like art. Not in the sense of painting and sculpture, but in the sense of assisted reproductive technologies. So what are assisted reproductive technologies? Assisted reproductive technologies are things like in vitro fertilization. And when you do in vitro fertilization, there''s very good reasons to do it. Sometimes you just can''t conceive otherwise. But when you do that, what you’re doing is separating sex, conception, baby. So you haven''t just taken control of when you have a baby, you''ve separated when the baby and where the baby is fertilized. So you''ve separated the baby from the body from the act. And as you''re thinking of other things we''ve been doing, think about twins. So you can freeze sperm, you can freeze eggs, you can freeze fertilized eggs. And what does that mean? Well, that''s a good thing if you''re a cancer patient. You''re about to go under chemotherapy or under radiation, so you save these things. You don''t irradiate them. But if you can save them and you can freeze them, and you can have a surrogate mother, it means that you''ve decoupled sex from time. It means you can have twins born -- oh, in 50 years? 

(Laughter) 

In a hundred years? Two hundred years? And these are three really profound changes that are not, like, future stuff. This is stuff we take for granted today. 

So this lifecode stuff turns out to be a superpower. It turns out to be this incredibly powerful way of changing viruses, of changing plants, of changing animals, perhaps even of evolving ourselves. It''s something that Steve Gullans and I have been thinking about for a while. 

Let''s have some risks. Like every powerful technology, like electricity, like an automobile, like computers, this stuff potentially can be misused. And that scares a lot of people. And as you apply these technologies, you can even turn human beings into chimeras. Remember the Greek myth where you mix animals? Well, some of these treatments actually end up changing your blood type. Or they''ll put male cells in a female body or vice versa, which sounds absolutely horrible until you realize, the reason you''re doing that is you''re substituting bone marrow during cancer treatments. So by taking somebody else''s bone marrow, you may be changing some fundamental aspects of yourself, but you''re also saving your life. And as you''re thinking about this stuff, here''s something that happened 20 years ago. 

This is Emma Ott. She''s a recent college admittee. She''s studying accounting. She played two varsity sports. She graduated as a valedictorian. And that''s not particularly extraordinary, except that she''s the first human being born to three parents. Why? Because she had a deadly mitochondrial disease that she might have inherited. So when you swap out a third person''s DNA and you put it in there, you save the lives of people. But you also are doing germline engineering, which means her kids, if she has kids, will be saved and won''t go through this. And [their] kids will be saved, and their grandchildren will be saved, and this passes on. 

That makes people nervous. So 20 years ago, the various authorities said, why don''t we study this for a while? There are risks to doing stuff, and there are risks to not doing stuff, because there were a couple dozen people saved by this technology, and then we''ve been thinking about it for the next 20 years. So as we think about it, as we take the time to say, "Hey, maybe we should have longer studies, maybe we should do this, maybe we should do that," there are consequences to acting, and there are consequences to not acting. Like curing deadly diseases -- which, by the way, is completely unnatural. It is normal and natural for humans to be felled by massive epidemics of polio, of smallpox, of tuberculosis. When we put vaccines into people, we are putting unnatural things into their body because we think the benefit outweighs the risk. Because we''ve built unnatural plants, unnatural animals, we can feed about seven billion people. We can do things like create new life-forms. And as you create new life-forms, again, that sounds terribly scary and terribly bothersome, until you realize that those life-forms live on your dining room table. Those flowers you''ve got on your dining room table -- there''s not a lot that''s natural about them, because people have been breeding the flowers to make this color, to be this size, to last for a week. You don''t usually give your loved one wildflowers because they don''t last a whole lot of time. 

What all this does is it flips Darwin completely on his head. See, for four billion years, what lived and died on this planet depended on two principles: on natural selection and random mutation. And so what lived and died, what was structured, has now been flipped on its head. And what we''ve done is created this completely parallel evolutionary system where we are practicing unnatural selection and non-random mutation. 

So let me explain these things. This is natural selection. This is unnatural selection. 

(Laughter) 

So what happens with this stuff is, we started breeding wolves thousands of years ago in central Asia to turn them into dogs. And then we started turning them into big dogs and into little dogs. But if you take one of the chihuahuas you see in the Hermès bags on Fifth Avenue and you let it loose on the African plain, you can watch natural selection happen. 

(Laughter) 

Few things on Earth are less natural than a cornfield. You will never, under any scenario, walk through a virgin forest and see the same plant growing in orderly rows at the same time, nothing else living there. When you do a cornfield, you''re selecting what lives and what dies. And you''re doing that through unnatural selection. It''s the same with a wheat field, it''s the same with a rice field. It''s the same with a city, it''s the same with a suburb. In fact, half the surface of Earth has been unnaturally engineered so that what lives and what dies there is what we want, which is the reason why you don''t have grizzly bears walking through downtown Manhattan. 

How about this random mutation stuff? Well, this is random mutation. This is Antonio Alfonseca. He''s otherwise known as the Octopus, his nickname. He was the Relief Pitcher of the Year in 2000. And he had a random mutation that gave him six fingers on each hand, which turns out to be really useful if you''re a pitcher. 

(Laughter) 

How about non-random mutation? A non-random mutation is beer. It''s wine. It''s yogurt. How many times have you walked through the forest and found all-natural cheese? Or all-natural yogurt? So we''ve been engineering this stuff. Now, the interesting thing is, we get to know the stuff better. We found one of the single most powerful gene-editing instruments, CRISPR, inside yogurt. And as we start engineering cells, we''re producing eight out of the top 10 pharmaceutical products, including the stuff that you use to treat arthritis, which is the number one best-selling drug, Humira. 

So this lifecode stuff. It really is a superpower. It really is a way of programming stuff, and there''s nothing that''s going to change us more than this lifecode. So as you''re thinking of lifecode, let''s think of five principles as to how we start guiding, and I''d love you to give me more. 

So, principle number one: we have to take responsibility for this stuff. The reason we have to take responsibility is because we''re in charge. These aren''t random mutations. This is what we are doing, what we are choosing. It''s not, "Stuff happened." It didn''t happen at random. It didn''t come down by a verdict of somebody else. We engineer this stuff, and it''s the Pottery Barn rule: you break it, you own it. 

Principle number two: we have to recognize and celebrate diversity in this stuff. There have been at least 33 versions of hominids that have walked around this Earth. Most all of them went extinct except us. But the normal and natural state of this Earth is we have various versions of humans walking around at the same time, which is why most of us have some Neanderthal in us. Some of us have some Denisova in us. And some in Washington have a lot more of it. 

(Laughter) 

Principle number three: we have to respect other people''s choices. Some people will choose to never alter. Some people will choose to alter all. Some people will choose to alter plants but not animals. Some people will choose to alter themselves. Some people will choose to evolve themselves. Diversity is not a bad thing, because even though we think of humans as very diverse, we came so close to extinction that all of us descend from a single African mother and the consequence of that is there''s more genetic diversity in 55 African chimpanzees than there are in seven billion humans. 

Principle number four: we should take about a quarter of the Earth and only let Darwin run the show there. It doesn''t have to be contiguous, doesn''t have to all be tied together. It should be part in the oceans, part on land. But we should not run every evolutionary decision on this planet. We want to have our evolutionary system running. We want to have Darwin''s evolutionary system running. And it''s just really important to have these two things running in parallel and not overwhelm evolution. 

(Applause) 

Last thing I''ll say. This is the single most exciting adventure human beings have been on. This is the single greatest superpower humans have ever had. It would be a crime for you not to participate in this stuff because you''re scared of it, because you''re hiding from it. You can participate in the ethics. You can participate in the politics. You can participate in the business. You can participate in just thinking about where medicine is going, where industry is going, where we''re going to take the world. It would be a crime for all of us not to be aware when somebody shows up at a swimming pool and says one word, just one word, if you don''t listen if that word is "lifecode." 

Thank you very much. 

(Applause) ');


--
-- PostgreSQL database dump complete
--


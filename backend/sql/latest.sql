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

ALTER TABLE ONLY public.transcript DROP CONSTRAINT transcript_id_fkey1;
ALTER TABLE ONLY public.transcript DROP CONSTRAINT transcript_id_fkey;
DROP INDEX public.en_idx;
ALTER TABLE ONLY public.transcript DROP CONSTRAINT transcript_pkey;
ALTER TABLE ONLY public.talk DROP CONSTRAINT talk_pkey;
ALTER TABLE ONLY public.talk DROP CONSTRAINT talk_id_name_key;
DROP TABLE public.transcript;
DROP TABLE public.talk;
DROP EXTENSION plpgsql;
DROP SCHEMA public;
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA public;


ALTER SCHEMA public OWNER TO postgres;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'standard public schema';


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: talk; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE talk (
    id smallint NOT NULL,
    name text,
    slug text,
    filmed_at timestamp with time zone,
    published_at timestamp with time zone,
    description text,
    image text,
    languages jsonb,
    media_slug text,
    media_pad real
);


ALTER TABLE talk OWNER TO postgres;

--
-- Name: transcript; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE transcript (
    id smallint NOT NULL,
    en_tsvector tsvector
);


ALTER TABLE transcript OWNER TO postgres;

--
-- Name: talk_id_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY talk
    ADD CONSTRAINT talk_id_name_key UNIQUE (id, name);


--
-- Name: talk_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY talk
    ADD CONSTRAINT talk_pkey PRIMARY KEY (id);


--
-- Name: transcript_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY transcript
    ADD CONSTRAINT transcript_pkey PRIMARY KEY (id);


--
-- Name: en_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX en_idx ON transcript USING gin (en_tsvector);


--
-- Name: transcript_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY transcript
    ADD CONSTRAINT transcript_id_fkey FOREIGN KEY (id) REFERENCES talk(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

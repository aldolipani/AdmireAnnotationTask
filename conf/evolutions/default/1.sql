# --- Created by Ebean DDL
# To stop Ebean DDL generation, remove this comment and start using Evolutions

# --- !Ups

create table annotation (
  id                        integer auto_increment not null,
  idSeed                    varchar(255),
  idEmbryon                 integer,
  idUser                    integer,
  idCategory                varchar(255),
  date_creation             TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint uq_annotation_1 unique (idSeed,idEmbryon,idUser),
  constraint pk_annotation primary key (id))
;

create table category (
  id                        varchar(255) not null,
  constraint pk_category primary key (id))
;

create table embryon (
  id                        integer auto_increment not null,
  id_embryon                varchar(255) not null,
  idSeed                    varchar(255),
  date_creation             TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint uq_embryon_1 unique (id_embryon,idSeed),
  constraint pk_embryon primary key (id))
;

create table log (
  id                        integer auto_increment not null,
  text                      TEXT not null,
  date_creation             TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint pk_log primary key (id))
;

create table seed (
  id                        varchar(255) not null,
  date_creation             TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint pk_seed primary key (id))
;

create table token (
  id                        varchar(255) not null,
  idUser                    integer,
  date_creation             TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint pk_token primary key (id))
;

create table user (
  id                        integer auto_increment not null,
  email                     varchar(255) not null,
  password                  varchar(255) not null,
  last_update               TIMESTAMP DEFAULT CURRENT_TIMESTAMP not null,
  constraint uq_user_email unique (email),
  constraint pk_user primary key (id))
;

alter table annotation add constraint fk_annotation_seed_1 foreign key (idSeed) references seed (id) on delete restrict on update restrict;
create index ix_annotation_seed_1 on annotation (idSeed);
alter table annotation add constraint fk_annotation_embryon_2 foreign key (idEmbryon) references embryon (id) on delete restrict on update restrict;
create index ix_annotation_embryon_2 on annotation (idEmbryon);
alter table annotation add constraint fk_annotation_user_3 foreign key (idUser) references user (id) on delete restrict on update restrict;
create index ix_annotation_user_3 on annotation (idUser);
alter table annotation add constraint fk_annotation_category_4 foreign key (idCategory) references category (id) on delete restrict on update restrict;
create index ix_annotation_category_4 on annotation (idCategory);
alter table embryon add constraint fk_embryon_seed_5 foreign key (idSeed) references seed (id) on delete restrict on update restrict;
create index ix_embryon_seed_5 on embryon (idSeed);
alter table token add constraint fk_token_user_6 foreign key (idUser) references user (id) on delete restrict on update restrict;
create index ix_token_user_6 on token (idUser);



# --- !Downs

SET FOREIGN_KEY_CHECKS=0;

drop table annotation;

drop table category;

drop table embryon;

drop table log;

drop table seed;

drop table token;

drop table user;

SET FOREIGN_KEY_CHECKS=1;


-- MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

-- Copyright © Josep Jesus Bigorra Algaba

-- mutastructura is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- mutastructura is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with mutastructura. If not, see <https://www.gnu.org/licenses/>.

CREATE TABLE "password_reset" (
	"user_id"	varchar(36) NOT NULL UNIQUE,
	"reset_token"	varchar(2048) NOT NULL,
	"created_at"	date NOT NULL
);

CREATE TABLE "project" (
	"id"	varchar(36),
	"human_id"	varchar(32) NOT NULL UNIQUE,
	"title"	varchar(256) NOT NULL UNIQUE,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"url"	AFTER title,
	"vcs_url"	AFTER url,
	"description"	AFTER url,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY("id")
);

CREATE TABLE "ticket" (
	"human_id"	integer,
	"id"	varchar(36) NOT NULL UNIQUE,
	"project_id"	varchar(36) NOT NULL,
	"title"	varchar(512) NOT NULL UNIQUE,
	"description"	varchar(4096),
	"ticket_status"	varchar(256) NOT NULL,
	"ticket_type"	varchar(256) NOT NULL,
	"created_at"	date NOT NULL,
	"updated_at"	date,
	"priority"	INTEGER,
	"deleted_at"	DATE DEFAULT NULL,
	PRIMARY KEY("human_id" AUTOINCREMENT)
);

CREATE TABLE "ticket_assignee" (
	"ticket_id"	varchar(36) NOT NULL,
	"project_id"	varchar(36) NOT NULL,  
	"user_id"	varchar(36) NOT NULL,
	PRIMARY KEY("ticket_id")
);

CREATE TABLE "user" (
	"id"	varchar(36),
	"username"	varchar(256) NOT NULL,
	"email"	varchar(256) NOT NULL,
	"password"	varchar(512) NOT NULL,
	"salt"	varchar(36) NOT NULL,
	"permissions"	varchar(4096),
	PRIMARY KEY("id")
);

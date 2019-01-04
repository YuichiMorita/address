-- データベース作成
-- CREATE DATABASE address ENCODING 'UTF-8';

-- このファイルを実行し、テーブルを作成
-- psql -U postgres -d address -f table.sql

CREATE TABLE user_types( --ユーザー種別
	id		serial		NOT NULL PRIMARY KEY,
	type		VARCHAR(100)	NOT NULL, --ユーザー種別[個人、法人]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW() --作成日時
);

CREATE TABLE users(	--ユーザーテーブル
	id		uuid		NOT NULL PRIMARY KEY,
	user_types_id	integer		NOT NULL references user_types(id) ON DELETE RESTRICT, --ユーザー種別[個人、法人]
	company 	VARCHAR(255),	--社名
	title 		VARCHAR(255),	--肩書き
	name 		VARCHAR(255)	NOT NULL, --名前
	memo 		TEXT,		--メモ
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE tell_types(	--電話種別テーブル
	id		serial		NOT NULL PRIMARY KEY,
	type		VARCHAR(100)	NOT NULL, --電話番号種別[自宅、勤務先、FAX、携帯電話など]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE tells(	--電話番号テーブル
	id		serial		NOT NULL PRIMARY KEY,
	users_id 	uuid	NOT NULL references users(id) ON DELETE CASCADE, --ユーザーID
	tell_stypes_id 	integer NOT NULL references telltypes(id) ON DELETE RESTRICT, --電話番号種別
	number		VARCHAR(20)	NOT NULL, --電話番号
	memo 		TEXT,		--メモ
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);


CREATE TABLE email_types(	--メール種別テーブル
	id		serial		NOT NULL PRIMARY KEY,
	type		VARCHAR(100)	NOT NULL, --メール種別[自宅、勤務先、携帯メールなど]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE emails(	--メールテーブル
	id		serial		NOT NULL PRIMARY KEY,
	users_id 	uuid NOT NULL references users(id) ON DELETE CASCADE, --ユーザーID
	email_types_id 	integer NOT NULL	references emailtypes(id) ON DELETE RESTRICT, --メール種別
	email		VARCHAR(255)	NOT NULL, --メールアドレス
	memo 		TEXT,		--メモ
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);


CREATE TABLE addr_types(	--住所種別テーブル
	id		serial		NOT NULL PRIMARY KEY,
	type		VARCHAR(100)	NOT NULL, --住所種別[請求先住所、配送先住所、自宅、勤務先など]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE addrs(	--住所テーブル
	id		serial		NOT NULL PRIMARY KEY,
	users_id 	uuid	NOT NULL 	references users(id) ON DELETE CASCADE, --ユーザーID
	addr_types_id 	integer	 NOT NULL references addr_types(id) ON DELETE RESTRICT, --住所種別
	postal 		VARCHAR(7),	--郵便番号
	pref 		VARCHAR(10),	--都道府県
	addr1 		VARCHAR(255),	--住所１
	addr2 		VARCHAR(255),	--住所２
	bld 		VARCHAR(255),	--建物
	place_name 	VARCHAR(255),	--場所の識別名[支社名、支店名、サロン名、工場名など]
	memo 		TEXT,		--メモ
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE url_types(	--url種別テーブル
	id		serial		NOT NULL PRIMARY KEY,
	type		VARCHAR(100)	NOT NULL, --url種別[ホームページ、ブログなど]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE urls( -- urlテーブル
	id		serial		NOT NULL PRIMARY KEY,
	users_id 	uuid	 NOT NULL 	references users(id) ON DELETE CASCADE, --ユーザーID
	url_types_id 	integer	NOT NULL 	references url_types(id) ON DELETE RESTRICT, --url種別
	url 		TEXT, 		--url
	memo 		TEXT,		--メモ
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE tags( --tagテーブル
	id		serial		NOT NULL PRIMARY KEY,
	tag		VARCHAR(255)	NOT NULL, --タグ[Reju、REX、仕入先、お歳暮、年賀状など]
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

CREATE TABLE users_tags(	--ユーザー タグの中間テーブル
	id		serial		NOT NULL PRIMARY KEY,
	tags_id		integer	NOT NULL references tags(id) ON DELETE CASCADE, --タグID
	users_id 	uuid	NOT NULL references users(id) ON DELETE CASCADE, --ユーザーID
	created_at 	TIMESTAMPTZ	NOT NULL DEFAULT NOW()  --作成日時
);

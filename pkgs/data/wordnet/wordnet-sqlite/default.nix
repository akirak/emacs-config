# Create a sqlite database from the data distributed at <http://www.semantilog.org/wn2sql.html>
# See <http://www.semantilog.org/license.html> for license.
{
  stdenv,
  sqlite,
}:
stdenv.mkDerivation {
  name = "wordnet-sqlite";

  buildInputs = [sqlite];

  src = builtins.fetchTarball {
    url = "http://www.semantilog.org/pub/wn2sql-0.99.4a.tar";
    sha256 = "12dm207y526vbs9jbkij6s63yk7m4gximvvznz9xnip3n4hn7s4r";
  };

  schema = ./schema.sql;

  phases = ["buildPhase" "installPhase"];

  files = ["word.bz2" "lexrel.bz2" "reltype.bz2" "synset.bz2" "sense.bz2" "lexname.bz2"];

  buildPhase = ''
    db=wordnet.db

    sqlite3 $db < $schema

    for file in $files
    do
      echo "Importing $file..."
      table=$(basename -s .bz2 $file)
      bunzip2 -c $src/$file | while read s; do
        echo "INSERT INTO $table VALUES $s;"
      done | sqlite3 $db
    done
  '';

  installPhase = ''
    cp wordnet.db $out
  '';
}

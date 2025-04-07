{ writers, python3Packages }:
writers.writePython3Bin "sqlglot-formatter"
  {
    libraries = [
      python3Packages.sqlglot
    ];
  }
  ''
    import sys
    import argparse
    import sqlglot


    def main():
        parser = argparse.ArgumentParser(description="Format SQL using sqlglot")
        parser.add_argument("--dialect", default=None, help="SQL dialect")
        args = parser.parse_args()

        dialect = args.dialect

        content = sys.stdin.read()
        try:
            formatted_sql_list = sqlglot.transpile(content, read=dialect,
                                                   pretty=True)
            formatted_sql = ';\n\n'.join(formatted_sql_list)
            if content.rstrip().endswith(';'):
                formatted_sql += ";"
            print(formatted_sql)
        except Exception as e:
            sys.stderr.write(f"Error: {e}\n")
            sys.exit(1)


    if __name__ == "__main__":
        main()
  ''

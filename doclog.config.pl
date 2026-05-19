project_name("org.revue-de-presse.bsky").
readme_file("README.md").
source_lib_folder("src").
websource("https://github.com/revue-de-presse/org.revue-de-presse.bsky/tree/main/src").
omit(["app", "chat", "com"]).
learn_pages_source_folder(".").
learn_pages_categories([]).
learn_pages([]).
base_url("/").

:- dynamic(copy_file/2).

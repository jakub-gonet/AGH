#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
// Napisz program, który rozpoczynając od katalogu podanego jako pierwszy
// parametr uruchomienia, idąc w głąb drzewa katalogów, znajdzie pliki
// zawierające łańcuch podany jako drugi parametr uruchomienia programu.
//
// Przeszukiwanie każdego z podkatalogów powinno odbyć się w osobnym
// procesie potomnym. Wydruk wyniku wyszukiwania poprzedź wypisaniem ścieżki
// względnej od katalogu podanego jako argument uruchomienia oraz numeru PID
// procesu odpowiedzialnego za przeglądanie określonego (pod)katalogu.
//
// Przeszukiwanie powinno obejmować pliki tekstowe i pomijać pliki
// binarne/wykonywalne/obiektowe etc.
// Program jako trzeci parametr powinien przyjmować maksymalną głębokość
// przeszukiwania licząc od katalogu podanego jako pierwszy parametr.

bool is_text_file(const struct dirent* dir_entry);
bool is_directory(const struct dirent* dir_entry);
bool is_curr_or_parent_dir(const struct dirent* dir_entry);
bool contains_pattern(const char* filepath, const char* pattern);

void walk(const char* path, const char* pattern, const int depth);

int main(int argc, char* argv[]) {
  assert(argc == 3 + 1);
  char* end;

  char* search_dir = argv[1];
  const char* pattern = argv[2];
  errno = 0;
  const long depth = strtol(argv[3], &end, 10);
  assert(errno == 0);

  walk(search_dir, pattern, depth);

  return 0;
}

void walk(const char* path, const char* pattern, const int depth) {
  printf("walk: %s, %s, %d\n", path, pattern, depth);
  if (depth == -1) {
    return;
  }
  chdir(path);
  if (errno != 0) {
    return;
  }
  DIR* dir_handle;
  dir_handle = opendir(".");
  if (dir_handle == NULL) {
    return;
  }
  struct dirent* dir;
  while ((dir = readdir(dir_handle)) != NULL) {
    if (is_curr_or_parent_dir(dir)) {
      continue;
    }
    if (is_directory(dir)) {
      pid_t pid = fork();
      if (pid == 0) {
        walk(dir->d_name, pattern, depth - 1);
        break;
      }
    } else if (is_text_file(dir) && contains_pattern(dir->d_name, pattern)) {
      printf("Found pattern in file: %s\n", dir->d_name);
    }
  }
  closedir(dir_handle);
}

bool is_text_file(const struct dirent* dir_entry) {
  const char* name = dir_entry->d_name;
  const size_t length = strlen(name);
  return length > 4 && !strcmp(name + length - 4, ".txt");
}

bool is_directory(const struct dirent* dir_entry) {
  return dir_entry->d_type == DT_DIR;
}

bool is_curr_or_parent_dir(const struct dirent* dir_entry) {
  const char* name = dir_entry->d_name;
  return strcmp(name, ".") == 0 || strcmp(name, "..") == 0;
}

bool contains_pattern(const char* filepath, const char* pattern) {
  char cwd[PATH_MAX];
  getcwd(cwd, sizeof(cwd));
  strcat(cwd, "/");
  strcat(cwd, filepath);
  printf("searching: %s\n", cwd);
  pid_t pid = vfork();
  if (pid == 0) {
    char* args[] = {"grep", (char*)pattern, cwd, "-q", NULL};
    execvp(args[0], args);
    printf("errno: %d\n", errno);
    assert(errno == 0);
  } else if (pid > 0) {
    int status;
    waitpid(pid, &status, 0);
    return WEXITSTATUS(status) == 0;
  }
  return false;
}

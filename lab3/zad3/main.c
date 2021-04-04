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
void remove_prefix(const int prefix_len, char* buffer);

void walk(const char* startpath,
          const char* path,
          const char* pattern,
          const int depth);

int main(int argc, char* argv[]) {
  assert(argc == 3 + 1);
  char* end;

  char* search_dir = argv[1];
  chdir(search_dir);
  if (errno != 0) {
    return 1;
  }
  char cwd[PATH_MAX];
  getcwd(cwd, sizeof(cwd));
  strcat(cwd, "/");

  const char* pattern = argv[2];
  errno = 0;
  const long depth = strtol(argv[3], &end, 10);
  assert(errno == 0);
  walk(cwd, ".", pattern, depth);

  return 0;
}

void walk(const char* startpath,
          const char* path,
          const char* pattern,
          const int depth) {
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
        walk(startpath, dir->d_name, pattern, depth - 1);
        break;
      }
    } else if (is_text_file(dir) && contains_pattern(dir->d_name, pattern)) {
      char cwd[PATH_MAX];
      getcwd(cwd, sizeof(cwd));
      strcat(cwd, "/");
      strcat(cwd, dir->d_name);
      remove_prefix(strlen(startpath), cwd);

      printf("[%d] Found pattern in file: %s\n", getpid(), cwd);
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
  pid_t pid = vfork();
  if (pid == 0) {
    char* args[] = {"grep", (char*)pattern, (char*)filepath, "-q", NULL};
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

void remove_prefix(const int prefix_len, char* buffer) {
  memmove(buffer, buffer + prefix_len, strlen(buffer) + 1 - prefix_len);
}

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include "vector.h"

// // DEBUG
// for (size_t i = 0; i < vec_get_size(rules); i++) {
//   printf("[rule %ld]\n", i);
//   for (size_t j = 0; j < vec_get_size(rules[i]); j++) {
//     printf("\t[%ld]\n", j);
//     for (size_t k = 0; k < vec_get_size(rules[i][j]); k++) {
//       printf("\t\t[%ld] %s\n", k, rules[i][j][k]);
//     }
//   }
// }
// // \DEBUG
typedef vec_type(char*) args_t;
typedef vec_type(args_t) rule_t;

rule_t prepare_next_cmd(char* line, vec_type(rule_t) rules);
rule_t parse_next_rule(char* line);
args_t split_into_args_arr(char* cmd_str);
void free_rules(vec_type(rule_t) rules);
void free_rule(rule_t rule);
void free_cmd(args_t cmd_with_args);
char* trim(char* str);
char* get_first_number(char* cmd);
void exec_rule(rule_t to_exec);

int main(int argc, char const* argv[]) {
  assert(argc == 1 + 1);

  vec_type(rule_t) rules = NULL;

  // open file for parsing
  FILE* file = fopen(argv[1], "r");
  assert(file != NULL);

  // parse file
  size_t n = 0;
  char* line = NULL;
  while (getline(&line, &n, file) != -1) {
    char* trimmed_line = trim(line);
    rule_t rule = parse_next_rule(trimmed_line);
    if (rule != NULL) {
      // add rule
      vec_append(rules, rule);
    } else if (strlen(trimmed_line) != 0) {
      // exec rule
      rule_t to_exec = prepare_next_cmd(trimmed_line, rules);
      // exec_rule(to_exec);
      vec_free(to_exec);
    }

    // free line
    free(line);
    line = NULL;
    n = 0;
  }

  free_rules(rules);
  free(line);
  fclose(file);
  return 0;
}

void exec_rule(rule_t to_exec) {
  int fd[2];
  pipe(fd);
  const size_t size = vec_get_size(to_exec);
  for (size_t i = 0; i < size; i++) {
    args_t args = to_exec[i];
    pid_t pid = fork();
    if (pid == 0) {
      if (i == 0) {
        close(fd[1]);
      } else if (i == size - 1) {
        close(fd[0]);
      }
      dup2(fd[0], STDIN_FILENO);
      dup2(fd[1], STDOUT_FILENO);

      execv(args[0], args);
    }
  }
  while (wait(NULL) > 0)
    ;
}

rule_t prepare_next_cmd(char* line, vec_type(rule_t) rules) {
  rule_t cmds_chain = NULL;
  do {
    char* next_cmd_str = strsep(&line, "|");
    errno = 0;
    long rule_no = strtol(get_first_number(next_cmd_str), NULL, 10) -
                   1;  // rules names are indexed from 1
    assert(errno == 0);
    rule_t rule = rules[rule_no];
    for (size_t i = 0; i < vec_get_size(rule); i++) {
      vec_append(cmds_chain, rule[i]);
    }

  } while (line != NULL);
  return cmds_chain;
}

rule_t parse_next_rule(char* line) {
  strsep(&line, "=");
  if (line == NULL) {
    // no '=' found, it's an exec rule
    return NULL;
  }
  line++;  // skip '='

  rule_t rule = NULL;
  do {
    char* next_cmd_str = strsep(&line, "|");
    next_cmd_str = trim(next_cmd_str);
    args_t cmd = split_into_args_arr(next_cmd_str);
    vec_append(rule, cmd);
  } while (line != NULL);

  return rule;
}

args_t split_into_args_arr(char* cmd_str) {
  args_t cmd = NULL;
  do {
    char* arg = strsep(&cmd_str, " ");
    char* arg_copy = strdup(arg);
    assert(arg_copy != NULL);
    vec_append(cmd, arg_copy);
  } while (cmd_str != NULL);
  vec_append(cmd, NULL);
  return cmd;
}

char* get_first_number(char* cmd) {
  cmd = trim(cmd);
  while (!isdigit(*cmd)) {
    ++cmd;
  }
  return cmd;
}

char* trim(char* str) {
  while (isspace(*str)) {
    ++str;
  }
  ssize_t last_char_idx = strlen(str) - 1;
  while (last_char_idx >= 0 && isspace(str[last_char_idx])) {
    str[last_char_idx--] = 0;
  }
  return str;
}

void free_rules(vec_type(rule_t) rules) {
  for (size_t i = 0; i < vec_get_size(rules); i++) {
    free_rule(rules[i]);
  }
  vec_free(rules);
}

void free_rule(rule_t rule) {
  for (size_t i = 0; i < vec_get_size(rule); i++) {
    free_cmd(rule[i]);
  }
  vec_free(rule);
}

void free_cmd(args_t cmd_with_args) {
  // cmd args array is terminated by NULL, we don't free it
  for (size_t i = 0; i < vec_get_size(cmd_with_args) - 1; i++) {
    free(cmd_with_args[i]);
  }
  vec_free(cmd_with_args);
}
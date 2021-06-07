#if !defined(COMMON_H)
#define COMMON_H

#define UNUSED(...) (void)(__VA_ARGS__)

#define GAME_SIZE 3
enum cell_type { _ = 0, X = -1, O = 1 };
typedef enum cell_type area_t[GAME_SIZE][GAME_SIZE];

#endif  // COMMON_H

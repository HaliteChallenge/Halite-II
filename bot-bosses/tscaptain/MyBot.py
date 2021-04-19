import hlt
import numpy as np
import pandas as pd
from time import time
import itertools
import os
import sys

args = ' '.join(sys.argv)
if 'define' in args:
    overrides = eval(args.split('define')[1].strip())

TIME_LIMIT = 1.9
nav_angles = np.array([-np.array(list(range(0, 35, 1)) + list(range(35, 120, 3))), 1+np.array(list(range(0, 35, 1)) + list(range(35, 120, 3)))]).T.flatten()
nav_rads = np.deg2rad(nav_angles)

def compute_distance_matrix(rows, cols): #warning, reverse compared to previous
    B = rows
    A = cols
    dx = A.x.values[None] - B.x.values[:, None]
    dy = A.y.values[None] - B.y.values[:, None]
    return np.sqrt(np.square(dx) + np.square(dy))

def ccw(A,B,C):
    return (C.y-A.y) * (B.x-A.x) > (B.y-A.y) * (C.x-A.x)

# Return true if line segments AB and CD intersect
def intersect(A,B,C,D):
    return ccw(A,C,D) != ccw(B,C,D) and ccw(A,B,C) != ccw(A,B,D)

def intersect_segment_circle(start, ex, ey, circle, *, fudge=1.0):
    sx = start.x
    sy = start.y
    worker_radius = start.radius
    cx = circle.x
    cy = circle.y
    cr = circle.radius + worker_radius + fudge

    dx = ex - sx
    dy = ey - sy

    a = dx ** 2 + dy ** 2
    b = (sx ** 2 + sy ** 2 - sx * cx - sy * cy + ex * (cx - sx) + ey * (cy - sy))

    t = np.clip(b / (a + 1e-8), 0.0, 1.0)

    closest_x = sx + dx * t
    closest_y = sy + dy * t

    closest_distance = (closest_x - cx) ** 2 + (closest_y - cy) ** 2

    return (closest_distance <= cr ** 2)


from collections import namedtuple


class Squad:
    def __init__(self, member_ids, accuracy=0.85, scale=1.25, fudge=0.15):
        self.accuracy = accuracy
        self.r = scale
        self.fudge = fudge
        self.member_ids = member_ids
        self.formation = scale * pd.DataFrame(np.array([
            [np.cos(np.deg2rad(30)) - np.sqrt(1 / 3), np.sin(np.deg2rad(30))],
            [-np.sqrt(1 / 3), 0],
            [np.cos(np.deg2rad(-30)) - np.sqrt(1 / 3), np.sin(np.deg2rad(-30))]
        ]))
        self.radius = scale * np.sqrt(1/3) + hlt.constants.SHIP_RADIUS # FIXME

    def move(self, bot, x, y):
        v = np.array([x, y])
        orig = bot.ships.loc[self.member_ids][['x', 'y']].copy()
        disp = orig.mean() - orig

        target = namedtuple('pos', 'x y radius')(*v, 0.5)
        worker = namedtuple('pos', 'x y radius')(*orig.mean().values, self.radius)

        v =  v - orig.mean()
        dist_target = np.sqrt(v.dot(v)) - 3.8
        try:
            theta = bot.find_clear_rads(target, worker, min(20.0, dist_target))[0]
        except:
            theta = np.arctan2(*(v / np.sqrt(v.dot(v)))[::-1])
        # if bot.turn_count < 2:
        #     theta = 1.0
        #     dist_target = 0.01
        #
        # if hasattr(self, 'theta') and bot.turn_count >= 2:
        #     dtheta = (theta - self.theta) % (2*np.pi)
        #     theta = self.theta + np.clip(dtheta, -0.5, 0.5)

        self.theta = theta
        # assert len(self.member_ids) == 3
        # R = np.array([[np.cos(theta), -np.sin(theta)], [np.sin(theta), np.cos(theta)]])
        beta = 0.0
        R = np.array([[np.cos(beta), -np.sin(beta)], [np.sin(beta), np.cos(beta)]])
        # ordering = [2,1,0]
        # self.ordering = ordering
        formation = self.formation.copy()
        formation = formation.iloc[:len(self.member_ids)]
        formation = formation - formation.mean()  # assumes we want it centered
        formation = formation.dot(R.T)
        formation.columns = ['x', 'y']

        # if not hasattr(self, 'ordering'):
        #     d = compute_distance_matrix(formation, disp)
        #
        #     perms = np.array(list(itertools.permutations(range(len(formation)))))
        #     def nonintersect(ordering):
        #         startpoints = orig.iloc[ordering]
        #         endpoints = orig.iloc[ordering] + disp.iloc[ordering] + formation.values
        #         return not (intersect(startpoints.iloc[ordering[0]], endpoints.iloc[ordering[0]], startpoints.iloc[ordering[1]], endpoints.iloc[ordering[1]]) or
        #                 intersect(startpoints.iloc[ordering[2]], endpoints.iloc[ordering[2]], startpoints.iloc[ordering[1]], endpoints.iloc[ordering[1]]) or
        #                 intersect(startpoints.iloc[ordering[0]], endpoints.iloc[ordering[0]], startpoints.iloc[ordering[2]], endpoints.iloc[ordering[2]]))
        #     ordering = perms[np.argmin([d[list(p)].trace() for p in perms if nonintersect(p)])]
        #     self.ordering = ordering
        # else:
        #     ordering = self.ordering
        #


        # ordering = [o for o in ordering if o <len(self.member_ids)]
        #
        # disp = disp.iloc[ordering]
        # orig = orig.iloc[ordering]

        found = False
        # direction = (v / np.sqrt(v.dot(v)))
        direction = np.array([np.cos(theta), np.sin(theta)])

        for vshift in np.transpose(direction[:, None] * np.linspace(min(8.5, dist_target), 2, 100)):
            dd = disp + formation.values + vshift
            intthrust = np.round(dd.apply(np.linalg.norm, axis=1, raw=True)).astype(int)

            if intthrust.max() > 7:
                continue
            intangle = np.round(np.rad2deg(dd.apply(lambda x: np.arctan2(*x[::-1]), axis=1, raw=True))).astype(int)
            shift = np.transpose([intthrust.values * np.cos(np.deg2rad(intangle.values)),
                                  intthrust.values * np.sin(np.deg2rad(intangle.values))])
            if len(self.member_ids)>1:
                if compute_distance_matrix((orig + shift), (orig + shift)).flat[[1, 2, 5][:len(self.member_ids)]].min() <= 2 * 0.5 + self.fudge:
                    continue
            if True or ((orig + shift).reset_index(drop=True).corrwith(formation)).min() >= self.accuracy: #FIXME
                found = True
                intangle = intangle % 360
                break
        if found:
            command_queue = [('t', *x) for x in zip(orig.index, intthrust, intangle)]
            return [' '.join(str(x) for x in command) for command in command_queue]
        else:
            return []

class Bot:
    def __init__(self, game, **overrides):
        self.game = game
        self.map = game.map
        self.my_id = self.map.my_id
        self.num_players = len(self.map.all_players())

        # Tunable target penalties (I need to fit these...)
        self.DOCKABLE_PLANET_PENALTY = 0.0  # except this should probably stay zero
        # self.MAX_COLLISION_DISTANCE = 7*13
        self.MAX_COLLISION_DISTANCE = 14
        self.THREAT_CUTOFF = 30
        self.DEFENDER_DISTANCE = 3.0
        if self.num_players == 2:
            self.ENEMY_SHIP_PENALTY = -20.0 #-40
            self.LOW_HP_ENEMY_SHIP_PENALTY = -5.0
            self.D_CENTER_PENALTY = 0.01
            self.DEFENSE_PENALTY = -45.0
        else:
            self.ENEMY_SHIP_PENALTY = -10.0  # or a function of player strength in current frame? maybe should be positive for 4p?
            self.LOW_HP_ENEMY_SHIP_PENALTY = -5.0
            self.D_CENTER_PENALTY = -1 / 3  # minus one unit penalty for three units distance from center
            self.DEFENSE_PENALTY = -45.0

        self.__dict__.update(**overrides)
        self.ships = pd.DataFrame(columns=['docked', 'enemy', 'health', 'x', 'y', 'dx', 'dy', 'dhealth'])
        self.turn_count = 0
        self.update(self.map)

    def passed_time_limit(self):
        return (time() - self.start_of_turn) > TIME_LIMIT

    def update(self, map):
        self.start_of_turn = time()
        self.map = map
        self.turn_count += 1

        # Ships (computing dx, dy, and dhealth might be a waste of time, ideally the strategy should not depend on that)
        ships = self.map._all_ships()
        ids = pd.Index([s.id for s in ships], name='id')
        try:
            old = self.ships.loc[ids]
            old.index = ids
        except:
            old = self.ships

        self.ships = pd.DataFrame(dict(
            enemy=[s.owner.id != self.my_id for s in ships],
            x=[s.x for s in ships],
            y=[s.y for s in ships],
            health=[s.health for s in ships],
            docked=[s.docking_status.value != 0 for s in ships]
        ), index=ids)

        self.full_docked = pd.Series([s.docking_status.value==2 for s in ships], index=ids)

        self.ships['dx'] = self.ships.x - old.x
        self.ships['dy'] = self.ships.y - old.y
        self.ships['dhealth'] = self.ships.health - old.health
        self.ships.fillna(0.0, inplace=True)
        self.ships.sort_index(inplace=True)

        # Planets
        planets = self.map.all_planets()
        ids = pd.Index([p.id for p in planets], name='id')
        self.planets = pd.DataFrame(dict(
            docks=[(p.num_docking_spots - len(p._docked_ship_ids)) if (p.owner is None or p.owner.id == self.my_id) else 0 for p in planets],
            x=[p.x for p in planets],
            y=[p.y for p in planets],
            radius=[p.radius for p in planets],
            prod=[self.full_docked.loc[p._docked_ship_ids].sum() for p in planets],
            owner_id=[p.owner.id if p.owner is not None else -1 for p in planets],
        ), index=ids)

        if not hasattr(self, 'ship_turns_till_spawn'):
            self.ship_turns_till_spawn = pd.Series(12, index=self.planets.index, name='ship_turns_till_spawn')
        self.ship_turns_till_spawn[self.ship_turns_till_spawn <= 0] = 12
        self.ship_turns_till_spawn = self.ship_turns_till_spawn.loc[self.planets.index]
        self.ship_turns_till_spawn -= self.planets['prod']
        self.ship_turns_till_spawn = (self.planets['owner_id'] == self.my_id) * self.ship_turns_till_spawn + (
                                                                                                             self.planets[
                                                                                                                 'owner_id'] != self.my_id) * 12
        self.planets['turns_till_spawn'] = np.ceil(self.ship_turns_till_spawn / np.clip(self.planets['prod'], 0.001, None))

        obstacles = self.ships[self.ships.docked & ~self.ships.enemy][['x', 'y']].assign(type='dock', radius=hlt.constants.SHIP_RADIUS)
        obstacles = obstacles.append(self.planets[['x','y','radius']].assign(type='planet'), ignore_index=True)
        self.obstacles = obstacles

        if self.turn_count < 2:
            if self.num_players != 2:
                self.rush = False
            else:
                enemy_pos = self.ships[self.ships.enemy][['x','y']].mean()
                my_pos = self.ships[~self.ships.enemy][['x', 'y']].mean()
                v = enemy_pos - my_pos
                if np.sqrt(np.square(v.x) + np.square(v.y)) < 120:
                    self.rush = True
                else:
                    self.rush = False


    def kamikaze(self, worker, target):
        angle = int(round(np.rad2deg(np.arctan2(target.y - worker.y, target.x - worker.x))))
        thrust = hlt.constants.MAX_SPEED
        self.command_queue.append(('t', worker.name, thrust, angle % 360))
        self.workers.drop(worker.name, inplace=True)

    def special_moves(self):
        # low hp kamakaze
        kamikaze_workers = self.workers[(self.workers.health < 128) | (np.abs(self.workers.health / self.workers.dhealth) < 2)]
        enemy_ships = self.ships[self.ships.enemy]
        d = compute_distance_matrix(kamikaze_workers, enemy_ships)

        sindex, tindex = np.indices(d.shape)

        for _ in range(len(kamikaze_workers)):
            if np.min(d) > hlt.constants.MAX_SPEED:
                break
            fi = np.argmin(d)
            wi = sindex.flat[fi]
            ti = tindex.flat[fi]

            d[wi, :] = np.inf
            d[:, ti] = np.inf

            self.kamikaze(kamikaze_workers.iloc[wi], enemy_ships.iloc[ti])

        # Other ideas:
        # clumping to squads
        # special opening moves (including keeping defense reserve)
        # en route dodging # TODO put overlapping enemy attack zones in as obstacles

    def get_targets(self):
        # +0.0 each open dock
        targets = list()
        for pid, target in self.planets.drop('docks', axis=1).iterrows():
            for _ in range(self.planets.docks.loc[pid]):
                targets.append(target)  # to_dict?
        targets = pd.DataFrame(targets)
        targets['type'] = 'open_dock'

        # -30.0 enemy docks
        enemy_docks = self.ships[self.ships.enemy & self.ships.docked][['x', 'y']]
        enemy_docks['type'] = 'enemy_dock'
        enemy_docks['radius'] = hlt.constants.SHIP_RADIUS
        targets = pd.concat([targets,enemy_docks])

        # -45.0 position between my dock and nearest enemy if nearest enemy is less than 30 and inbound (or less than 5 stationary) (based on threat ship count, not dock count)
        threats = self.ships[self.ships.enemy & ~self.ships.docked]
        my_docks = self.ships[~self.ships.enemy & self.ships.docked]
        if len(threats) and len(my_docks):
            dx = threats.x.values[None] - my_docks.x.values[:, None]
            dy = threats.y.values[None] - my_docks.y.values[:, None]
            d = np.sqrt(np.square(dx) + np.square(dy))

            mask = (d < self.THREAT_CUTOFF).any(axis=0)

            targeted_docks = my_docks.iloc[d.argmin(axis=0)]

            targeted_docks = targeted_docks[mask]
            threats = threats[mask]
            if len(threats):
                defense_targets = targeted_docks[['x', 'y']] + self.DEFENDER_DISTANCE * (
                threats[['x', 'y']].values - targeted_docks[['x', 'y']].values) /d.min(axis=0)[mask.nonzero()[0],None]
                defense_targets['radius'] = 0.0
                defense_targets['type'] = 'defense'
                targets = pd.concat([targets, defense_targets])

        return pd.DataFrame(targets)

    def send_to(self, wi, ti, min_distance=3.0, smash=False):

        worker = self.workers.iloc[wi]
        target = self.targets.iloc[ti]

        if target['type'] == 'open_dock' and self.line_distances[wi, ti] < hlt.constants.DOCK_RADIUS:
            self.command_queue.append(('d', worker.name, target.name))
            if worker.name >= 0:
                self.obstacles = self.obstacles.append(dict(x=worker.x, y=worker.y, radius=worker.radius, type='dock'), ignore_index=True)
            return True
        else:
            if smash or target['type'] == 'defense':
                min_distance = 0.0
            d = round(min(self.line_distances[wi, ti] - min_distance, self.MAX_COLLISION_DISTANCE))

            rads = self.find_clear_rads(target, worker, d)

            if len(rads):
                angle = int(round(np.rad2deg(rads[0])))
                thrust = hlt.constants.MAX_SPEED if smash else int(round(min(d, hlt.constants.MAX_SPEED)))
                thrust = max(0, thrust)
                for cmd in self.command_queue:
                    assert cmd[1] != worker.name
                self.command_queue.append(('t', worker.name, thrust, angle % 360))

                if worker.name >= 0:
                    # add mid point obstacle  # would higher res or better logic here be a significant improvement?
                    # x = worker.x + np.cos(rads[0]) * thrust/2
                    # y = worker.y + np.sin(rads[0]) * thrust/2
                    # self.obstacles = self.obstacles.append(dict(x=x, y=y, radius=thrust/2 + worker.radius, type='ship'), ignore_index=True)

                    #FIXME, testing end-only obstacles
                    x = worker.x + np.cos(rads[0]) * thrust
                    y = worker.y + np.sin(rads[0]) * thrust
                    self.obstacles = self.obstacles.append(dict(x=x, y=y, radius=worker.radius, type='ship'), ignore_index=True)

                return True
            else:
                return False

    def find_clear_rads(self, target, worker, d):
        obstacles = self.obstacles.copy()
        obstacles = obstacles[np.sqrt(np.square(obstacles.x - worker.x) + np.square(obstacles.y - worker.y)) - obstacles.radius - worker.radius <= d]
        rads = (np.arctan2(target.y - worker.y, target.x - worker.x) + nav_rads)
        ex = worker.x + np.cos(rads) * d
        ey = worker.y + np.sin(rads) * d
        for _, obstacle in obstacles.iterrows():
            mask = ~intersect_segment_circle(worker, ex, ey, obstacle)
            rads = rads[mask]
            ex = ex[mask]
            ey = ey[mask]
        return rads

    def update_penalties(self, wi, ti):
        # Update distances here (could add penalty updates based on map and/or command_queue, e.g. additionals logic)
        self.distances[wi, :] = np.inf
        self.distances[:, ti] += 1000

    def compute_distances(self):
        # Compute distances (this could be updated with expected path distance)
        dx = self.targets.x.values[None] - self.workers.x.values[:, None]
        dy = self.targets.y.values[None] - self.workers.y.values[:, None]
        self.line_distances = np.sqrt(np.square(dx) + np.square(dy)) - self.targets.radius.values  # shouldn't need to account for ship radius
        self.distances = self.line_distances.copy()

    def apply_distance_penalties(self):
        # for name, penalty in TYPE_PENALTY.items():
        #     self.distances[:, (self.targets['type'] == name).values] += penalty

        self.distances[:, (self.targets['type'] == 'enemy_dock').values] += np.where(self.workers.health.values[:, None] >= 128, self.ENEMY_SHIP_PENALTY, self.LOW_HP_ENEMY_SHIP_PENALTY)
        self.distances[:, (self.targets['type'] == 'defense').values] += self.DEFENSE_PENALTY * (self.workers.health.values[:, None]/255)
        self.distances[self.workers.index.values < 0, :] += np.clip(7 * self.turns_till_spawn.values - 6.0, 0.0, None)[:, None]
        d_center = np.sqrt(np.square(self.targets.x.values - self.map.width/2) + np.square(self.targets.y.values - self.map.height/2))
        self.distances += self.D_CENTER_PENALTY * d_center

    def get_rush_commands(self):
        global nav_angles,nav_rads

        if not hasattr(self, 'squad'):
            nav_angles = np.array([-np.array(list(range(0, 175, 1))),
                                   1 + np.array(list(range(0, 175, 1)))]).T.flatten()
            nav_rads = np.deg2rad(nav_angles)
            self.squad = Squad(self.ships[~self.ships.enemy].sort_values('y', ascending=False).index, scale=max(1.25, 8.5 - self.turn_count**2))
        else:
            # ordering = self.squad.ordering
            self.squad = Squad(self.ships[~self.ships.enemy].sort_values('y', ascending=False).index, scale=max(1.25, 8.5 - self.turn_count**2))
            # self.squad.ordering = ordering
        pos = self.ships.loc[self.squad.member_ids][['x','y']].mean()
        enemy_ships = self.ships[self.ships.enemy]
        target = enemy_ships.iloc[np.argmin(np.sqrt(np.square(pos.x - enemy_ships.x.values) + np.square(pos.y-enemy_ships.y.values)))]
        return self.squad.move(self, target.x, target.y)

    def get_commands(self):

        self.command_queue = []
        if self.rush:
            return self.get_rush_commands()

        self.targets = self.get_targets()  # df with x,y,type,radius
        self.workers = self.ships[~(self.ships.enemy | self.ships.docked)]

        self.special_moves() # warning, removes workers

        unspawned_workers = self.planets[(self.planets.owner_id == self.my_id)].copy()
        unspawned_workers.index = -(unspawned_workers.index + 1)  # have negative ids
        unspawned_workers['docked'] = False
        unspawned_workers['enemy'] = False
        unspawned_workers['health'] = 255
        unspawned_workers['dx'] = 0.0
        unspawned_workers['dy'] = 0.0
        unspawned_workers['dhealth'] = 0.0

        from_center = unspawned_workers[['x', 'y']] - np.array([self.map.width / 2, self.map.height / 2])
        from_center = (
        from_center.values / np.sqrt(np.square(from_center.x.values) + np.square(from_center.y.values))[:, None])

        unspawned_workers[['x', 'y']] = unspawned_workers[['x', 'y']] - (
                                                                        unspawned_workers.radius.values + hlt.constants.SPAWN_RADIUS)[
                                                                        :, None] * from_center
        self.turns_till_spawn = unspawned_workers.turns_till_spawn
        self.workers = self.workers.append(unspawned_workers[self.workers.columns])
        self.workers['radius'] = hlt.constants.SHIP_RADIUS

        self.compute_distances() # workers X targets
        self.apply_distance_penalties()

        sindex, tindex = np.indices(self.distances.shape)

        for _ in range(len(self.workers)):
            if self.passed_time_limit() or len(self.targets) == 0 or len(self.workers) == 0:
                break

            fi = np.argmin(self.distances)
            wi = sindex.flat[fi]
            ti = tindex.flat[fi]

            self.update_penalties(wi, ti)

            self.send_to(wi, ti)

        self.command_queue = [x for x in self.command_queue if x[1] >= 0] # filter unspawned
        return [' '.join(str(x) for x in command) for command in self.command_queue]

if __name__ == '__main__':
    debug = False
    bot_name = 'MyBot'
    game = hlt.Game(bot_name)
    if not debug:
        sys.stderr = open(os.devnull, mode='w') # prevent warning messages from breaking bot
        bot = Bot(game)
        while True:
            game_map = game.update_map()
            bot.update(game_map)
            command_queue = bot.get_commands()
            game.send_command_queue(command_queue)
    else:
        import pickle

        bot = Bot(game)
        turn_count =0
        with open('{}.{}.pkl'.format(bot_name,turn_count), mode='wb') as file:
            pickle.dump(bot, file)
        while True:
            turn_count += 1
            game_map = game.update_map()
            try:
                bot.update(game_map)
                command_queue = bot.get_commands()

                with open('{}.{}.pkl'.format(bot_name,turn_count), mode='wb') as file:
                    pickle.dump(bot, file)
            except Exception as e:
                with open('{}.err.pkl'.format(bot_name), mode='wb') as file:
                    pickle.dump(bot, file)
                raise e
            game.send_command_queue(command_queue)

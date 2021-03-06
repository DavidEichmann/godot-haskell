module Godot.Core.GodotAnimationPlayer where
import Data.Coerce
import Foreign.C
import Godot.Internal.Dispatch
import System.IO.Unsafe
import Godot.Gdnative.Internal
import Godot.Gdnative.Types
import Godot.Api.Auto

pattern ANIMATION_PROCESS_MANUAL :: Int

pattern ANIMATION_PROCESS_MANUAL = 2

pattern ANIMATION_PROCESS_PHYSICS :: Int

pattern ANIMATION_PROCESS_PHYSICS = 0

pattern ANIMATION_PROCESS_IDLE :: Int

pattern ANIMATION_PROCESS_IDLE = 1

caches_cleared :: Signal GodotAnimationPlayer
caches_cleared = Signal "caches_cleared"

animation_started :: Signal GodotAnimationPlayer
animation_started = Signal "animation_started"

animation_changed :: Signal GodotAnimationPlayer
animation_changed = Signal "animation_changed"

animation_finished :: Signal GodotAnimationPlayer
animation_finished = Signal "animation_finished"
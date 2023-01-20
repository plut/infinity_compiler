use crate::{Resref,Strref};
use macros::{Pack, Table, produce_resource_list};
#[derive(Debug,Pack,Table)]
#[table(item_effects,itemref,items)] pub struct ItemEffect {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, i64, r#"references "item_abilities"("abref")"#)]
	pub opcode: u16, //opcode,
	pub target: u8, // EffectTarget,
	pub power: u8,
	pub parameter1: u32,
	pub parameter2: u32,
	pub timing_mode: u8, // TimingMode,
	pub dispel_mode: u8, // DispelMode,
	pub duration: u32,
	pub proba1: u8,
	pub proba2: u8,
	pub resource: Resref,
	pub dice_thrown: i32,
	pub dice_sides: i32,
	pub saving_throw_type: u32,
	pub saving_throw_bonus: i32,
	pub stacking_id: u32,
}
#[derive(Debug,Pack,Table)]
#[table(item_abilities,itemref,items)] pub struct ItemAbility {
#[column(itemref, Option<Resref>, r#"references "items"("itemref")"#)]
#[column(abref, auto, "primary key")]
	attack_type: u8, // AttackType,
	must_identify: u8,
	location: u8,
	alternative_dice_sides: u8,
	use_icon: Resref,
	target_type: u8, // TargetType,
	target_count: u8,
	range: u16,
	launcher_required: u8,
	alternative_dice_thrown: u8,
	speed_factor: u8,
	alternative_damage_bonus: u8,
	thac0_bonus: u16,
	dice_sides: u8,
	primary_type: u8,
	dice_thrown: u8,
	secondary_type: u8,
	damage_bonus: u16,
	damage_type: u16, // DamageType,
#[column(false)] pub effect_count: u16, // = 0,
#[column(false)] pub effect_index: u16, // = 0,
	max_charges: u16,
	depletion: u16,
	flags: u32,
	projectile_animation: u16,
	overhand_chance: u16,
	backhand_chance: u16,
	thrust_chance: u16,
	is_arrow: u16,
	is_bolt: u16,
	is_bullet: u16,
}
#[derive(Debug,Pack,Table)]
#[table(items,itemref,items)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref, "primary key")]
	unidentified_name: Strref,
	pub name: Strref,
	replacement: Resref,
	flags: u32, // ItemFlags,
	itemtype: u16, // ItemType,
	usability: u32, // UsabilityFlags,
	animation: u16, // StaticString<2>,
	min_level: u16,
	min_strength: u16,
	min_strengthbonus: u8,
	kit1: u8,
	min_intelligence: u8,
	kit2: u8,
	min_dexterity: u8,
	kit3: u8,
	min_wisdom: u8,
	kit4: u8,
	min_constitution: u8,
	proficiency: u8, // WProf,
	min_charisma: u16,
	price: u32,
	stack_amount: u16,
	inventory_icon: Resref,
	lore: u16,
	ground_icon: Resref,
	weight: i32,
	unidentified_description: Strref,
	description: Strref,
	description_icon: Resref,
	enchantment: i32,
#[column(false)] pub abilities_offset: u32,
#[column(false)] pub abilities_count: u16,
#[column(false)] pub effect_offset: u32,
#[column(false)] pub effect_index: u16,
#[column(false)] pub equip_effect_count: u16,
}

// This last invocation closes the list of resources above, generating:
//  - the `ByResource<T>` type constructor,
//  - its implementation of `iter()` and an iterator
//  - and the constant holding the parent resources.
produce_resource_list!();

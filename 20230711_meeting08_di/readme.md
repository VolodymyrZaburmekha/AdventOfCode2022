
### 2023.07.12 Meeting 8 Dependency Injection

- https://fsharpforfunandprofit.com/posts/dependency-injection-1/ 2016
	- Functional approaches to dependency injection 
- https://fsharpforfunandprofit.com/posts/dependencies/ 2020
	- Six approaches to dependency injection
	- links to other materials 
- https://blog.ploeh.dk/2017/01/27/from-dependency-injection-to-dependency-rejection/
	- From dependency injection to dependency rejection
	- https://www.youtube.com/watch?v=xG5qP5AWQws Mark Seemann — From dependency injection to dependency rejection
- https://www.bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/
	- Dealing with complex dependency injection in F#
- https://www.compositional-it.com/news-blog/refactoring-dependencies-in-f/ 
	- Refactoring dependencies in F#
- DI in functional TypeScript code

```typescript
export async function runMigration(migration: Migration, deps =
    { logger: appState.logger, getMigrationByName, insertMigration, updateMigration }) {

    let migrationDb = await deps.getMigrationByName(migration.name);
    try {
        if (!migrationDb) {  
            await runCreateMethod(migration, deps);
            migrationDb = await deps.getMigrationByName(migration.name);
        }

        await runUpdateFromMethods(migration, migrationDb, deps);
    } catch (err) {
        deps.logger.info(`migration '${migration.name}' failed! ${err}`);
        throw err;
    }
}

function getMigrationByName(name: string) {
    return appState.mongo.migrations.findOne<MigrationDb>({ _id: name });
}
async function insertMigration(migrationDb: MigrationDb) {
    await appState.mongo.migrations.insertOne(migrationDb);
}
async function updateMigration(migrationDb: MigrationDb) {
    await appState.mongo.migrations.updateOne({ _id: migrationDb._id }, { $set: migrationDb });
}
```
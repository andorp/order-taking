
export async function getIdentifier() {
  return new Promise((resolve, reject) => resolve(String(Math.random())));
}

export async function postData(url = '', data = {}) {
  const response = await fetch(url, {
    method: 'POST',
    cache: 'no-cache',
    body: JSON.stringify(data)
  });
  return response.json();
}
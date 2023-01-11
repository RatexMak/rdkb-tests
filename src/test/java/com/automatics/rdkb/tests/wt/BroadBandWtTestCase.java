package com.automatics.rdkb.tests.wt;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.wt.object.Station;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.wt.service.WtClientUtils;

public class BroadBandWtTestCase extends AutomaticsTestBase {

	/** URL for wt client */
	String[] wtDetail = null;
	boolean isWtClientPresent = false;
	String wtSimulatorBaseUrl;

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WTTEST-1000")

	public void testVerifyWtClientConnection(Dut device) {

		String testCaseId = "TC-RDKB-WTTEST-100";
		String errorMessage = null;

		Map<String, String> extraProps;

		/** Test step **/
		String step = "";

		/** step number */
		int stepNumber = 1;
		boolean status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"PRECONDITION 1: DESCRIPTION : Verify if the setup has candella clients and has got WISST services deployed and end point configured in CATS .");
		LOGGER.info(
				"PRECONDITION 1: ACTION :  Verify if the setup has candella clients and has got WISST services deployed and end point configured in CATS . ");
		LOGGER.info(
				"PRECONDITION 1: EXPECTED : Verify if the setup has candella clients and has got WISST services deployed and end point configured in CATS . ");
		LOGGER.info("**********************************************************************************");

		errorMessage = "Wt client URL is not configured or not in valid format. Skipping wt client execution";
		try {
			// wtSimulatorBaseUrl - WT base url (fetch from device extra properties of
			// device)
			extraProps = ((Device) device).getExtraProperties();
			wtSimulatorBaseUrl = extraProps.get("wtSimulatorBaseUrl");
			isWtClientPresent = CommonMethods.isNotNull(wtSimulatorBaseUrl);
			LOGGER.info("wtSimulatorBaseUrl is " + wtSimulatorBaseUrl);

		} catch (Exception e) {
			LOGGER.error("Exception occured while trying to get URL from account", e);
		}

		isWtClientPresent = true;

		String[] webpaList = { BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE };

		List<Station> stations = new ArrayList<Station>();
		BroadBandResultObject result = new BroadBandResultObject();
		try {
			if (isWtClientPresent) {

				Map<String, String> webpaValues = tapEnv.executeMultipleWebPaGetCommands(device, webpaList);

				// base url to wt-service will be fetched from AutomaticsProps
				String wtBaseUrl = AutomaticsPropertyUtility.getProperty("wt.service.base.url");

				LOGGER.info("wtBaseUrl is " + wtBaseUrl);

				LOGGER.info("**********************************************************************************");
				LOGGER.info("PRECONDITION 2 : DESCRIPTION : Attempt to fetch Station details before creating Station ");
				LOGGER.info("PRECONDITION 2 : ACTION : Fetch availbale stationdetails");
				LOGGER.info("PRECONDITION 2 : EXPECTED : There should not be a client station");
				LOGGER.info("**********************************************************************************");

				stations = WtClientUtils.getAllStationsDetails(wtSimulatorBaseUrl, wtBaseUrl);

				for (Station station : stations) {
					LOGGER.info("************************");

					LOGGER.info("Station Alias: " + station.getAlias());
					LOGGER.info("Station MAC: " + station.getMac());
					LOGGER.info("Station SSID: " + station.getSsid());
					LOGGER.info("Station Port: " + station.getPort());
					LOGGER.info("Station Mode: " + station.getMode());
					LOGGER.info("Station Status: " + station.getStatus());
					LOGGER.info("Station Down: " + station.getDownStatus());
					LOGGER.info("Station parentDev: " + station.getParentDev());
					LOGGER.info("Station ethernetInterface: " + station.getEthernetInterface());

					LOGGER.info("************************");
				}

				stepNumber = 1;
				step = "S" + stepNumber;
				status = false;
				errorMessage = "Attempt to initiate new candela client and connected with Broadband device has failed";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Create 2 new client station on candella clients, connect to wifi and validate connectivity ");
				LOGGER.info("STEP " + stepNumber
						+ " : ACTION : Create new client stations for each band with existing station as model "
						+ "and connect those stations to it respective band");
				LOGGER.info(
						"STEP " + stepNumber + " : EXPECTED : The client stations should be connected successfully");
				LOGGER.info("**********************************************************************************");
				// Create station
				status = WtClientUtils.createNewStations(
						webpaValues.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID),
						webpaValues.get(
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE),
						wtBaseUrl, WiFiFrequencyBand.WIFI_BAND_2_GHZ, 1, wtSimulatorBaseUrl);

				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

				status = WtClientUtils.createNewStations(
						webpaValues.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID),
						webpaValues.get(
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE),
						wtBaseUrl, WiFiFrequencyBand.WIFI_BAND_5_GHZ, 1, wtSimulatorBaseUrl);

				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("status :  " + status);

				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " a : ACTUAL : Successfully created 2 new client station on candela client");

					stations = WtClientUtils.getAllStationsDetails(wtSimulatorBaseUrl, wtBaseUrl);

					for (Station station : stations) {
						LOGGER.info("************************");

						LOGGER.info("Station Alias: " + station.getAlias());
						LOGGER.info("Station MAC: " + station.getMac());
						LOGGER.info("Station SSID: " + station.getSsid());
						LOGGER.info("Station Port: " + station.getPort());
						LOGGER.info("Station Mode: " + station.getMode());
						LOGGER.info("Station Status: " + station.getStatus());
						LOGGER.info("Station Down: " + station.getDownStatus());
						LOGGER.info("Station parentDev: " + station.getParentDev());
						LOGGER.info("Station ethernetInterface: " + station.getEthernetInterface());

						LOGGER.info("************************");
					}

					// Connect

					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
					LOGGER.info(" Going to check whether created stations are connected to gateway");
					for (Station station : stations) {
						result = WtClientUtils.verifyDevicesAreConnectedToGateway(device, tapEnv, station, true);
						if (result.isStatus()) {
							LOGGER.info("STEP " + stepNumber + " : ACTUAL :Station with MAC " + station.getMac()
									+ " is connected to Gateway");
						} else {
							LOGGER.error("Station with MAC " + station.getMac() + " is  not connected to Gateway");
							errorMessage = result.getErrorMessage();
							LOGGER.error(result.getErrorMessage());
						}
					}

				} else {
					errorMessage = "Fail to create new client station on candela client";
					LOGGER.error("STEP " + stepNumber + "  : ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

				stepNumber = 2;
				step = "S" + stepNumber;
				status = false;
				errorMessage = "Attempt to delete exsisting station failed";
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP " + stepNumber + ": DESCRIPTION : Delete stations on candela that are created in step 1");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Delete all stations on candella client");
				LOGGER.info("STEP " + stepNumber + " : EXPECTED : The client stations should be deleted successfully");
				LOGGER.info("**********************************************************************************");

				// delete station

				for (Station station : stations) {

					String deleteStatus = WtClientUtils.deleteStation(station.getAlias(), wtSimulatorBaseUrl,
							wtBaseUrl);
					status = CommonMethods.isNotNull(deleteStatus);
					if (status) {

						LOGGER.info("Station Alias: " + station.getAlias());
						LOGGER.info("Station MAC: " + station.getMac());
						LOGGER.info("Station SSID: " + station.getSsid());
						LOGGER.info("Station Port: " + station.getPort());
						LOGGER.info("Station Mode: " + station.getMode());
						LOGGER.info("Station Status: " + station.getStatus());
						LOGGER.info("Station Down: " + station.getDownStatus());
						LOGGER.info("Station parentDev: " + station.getParentDev());
						LOGGER.info("Station ethernetInterface: " + station.getEthernetInterface());

						LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully deleted Station with ID :"
								+ station.getAlias());

					} else {
						LOGGER.error("STEP " + stepNumber + " : ACTUAL : Failed to delete Station with ID :"
								+ station.getAlias());
					}
				}
				tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

				LOGGER.info("Trying to get Station details after deleting Stations");
				stations = WtClientUtils.getAllStationsDetails(wtSimulatorBaseUrl, wtBaseUrl);

				for (Station station : stations) {
					LOGGER.info("************************");

					LOGGER.info("Station Alias: " + station.getAlias());
					LOGGER.info("Station MAC: " + station.getMac());
					LOGGER.info("Station SSID: " + station.getSsid());
					LOGGER.info("Station Port: " + station.getPort());
					LOGGER.info("Station Mode: " + station.getMode());
					LOGGER.info("Station Status: " + station.getStatus());
					LOGGER.info("Station Down: " + station.getDownStatus());
					LOGGER.info("Station parentDev: " + station.getParentDev());
					LOGGER.info("Station ethernetInterface: " + station.getEthernetInterface());

					LOGGER.info("************************");
				}

			} else {
				LOGGER.error("No WT clients are available to validate..");
			}
		} catch (Exception e) {
			LOGGER.info("Exception occured while trying to connect candela client", e);
		}
	}

}

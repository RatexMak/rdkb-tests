package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.List;
import java.util.Map;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWiFiConnectedClientTests extends AutomaticsTestBase {
	
    /**
     * Test to verify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is
     * disabled
     * <ol>
     * <li>STEP 1:Disable Private 2.4 GHz SSID via WebPA</li>
     * <li>STEP 2:Verify the Private 2.4 GHz SSID enabled status via WebPA</li>
     * <li>STEP 3:Verify the Private 5 GHz SSID enabled status via WebPA</li>
     * <li>STEP 4: Connect the device to 5 GHz SSID and verify connection status</li>
     * <li>STEP 5:Verify whether interface got the correct IPv4 address.</li>
     * <li>STEP 6:Verify whether interface got the correct IPv6 address.</li>
     * <li>STEP 7: Verify whether you have connectivity using that particular interface using IPV4</li>
     * <li>STEP 8: Verify whether you have connectivity using that particular interface using IPV6</li>
     * <li>STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value
     * of\"</li>
     * <li>STEP 10:Verify dmcli eRT getv Device.WiFi. - has to give more than 23 params.</li>
     * <li>Postcondition1:Revert WIFI SSID to its original state</li>
     * </ol>
     * 
     * @param Dut
     *            {@link device}
     * @author anandam.s
     * @refactor Athira
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-4001")
    public void testVerifyConnectivityOfClientDeviceWhichConnectedWith5GhzOnlyEnabledRdkbGateway(Dut device) {
	// String to store the test case status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-WIFI-401";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// connected device to be verified
	Dut connectedDeviceActivated = null;
	String deviceDateTime = null;
	try {

	    LOGGER.info("STARTING TEST CASE: " + testId);

	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Test to Verify that LAN side 5GHz wireless CPE gets an IP address when only 5 GHZ radio is enabled in Cable modem");
	    LOGGER.info("STEP 1:Disable Private 2.4 GHz SSID via WebPA");
	    LOGGER.info("STEP 2:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 3:Verify the Private 5 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 4: Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("STEP 5:Verify whether interface got the correct IPv4  address.");
	    LOGGER.info("STEP 6:Verify whether interface got the correct IPv6  address.");
	    LOGGER.info("STEP 7: Verify whether you have connectivity using that particular interface using IPV4 ");
	    LOGGER.info("STEP 8: Verify whether you have connectivity using that particular interface using IPV6 ");
	    LOGGER.info(
		    "STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
	    LOGGER.info("STEP 10:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
	    LOGGER.info("Postcondition1:Revert WIFI SSID to its original state");
	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1 Disable Private 2.4 GHz SSID via WebPA
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Description :Disable Private 2.4 GHz SSID via WebPA");
	    LOGGER.info("STEP 1: Action: Execute command Private 2.4 GHz SSID via WebPA "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    LOGGER.info(
		    "STEP 1:EXPECTED: Device should disable the Private 2.4 GHZ SSID and WebPA command return success response.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Private 2.4 GHZ SSID is not disabled ";
	    // Disable 2.4ghz radio
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)
		    && BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
			    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, false);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successsfully Disabled Private 2.4 GHz SSID via WebPA");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2 Verify the 2.4 GHz SSID enabled status via WebPA
	     */
	    testStepNumber = "s2";
	    status = false;
	    errorMessage = "Private 2.4 GHZ SSID is not disabled ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2:DESCRIPTION:Verify the 2.4 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 2.4 GHz SSID enabled status");
	    LOGGER.info("STEP 2 :EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Down\"");
	    LOGGER.info("**********************************************************************************");
	    // verify 2.4ghz radio status
	    Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
	    String radioStatus = radioSatusForAllBands
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
	    if (null != radioStatus) {
		errorMessage = "Verification of 2.4Ghz Private SSID status failed. EXPECTED ssid status is : "
			+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved " + radioStatus
			+ " value from WebPa command.";
		status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
	    } else {
		errorMessage = "Unable to fetch the 2.4Ghz Private SSID status using Webpa paramete"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successsfully Verified the 2.4 GHz SSID enabled status via WebPA");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3 Verify the 5 GHz SSID enabled status via WebPA
	     */
	    testStepNumber = "s3";
	    status = false;
	    radioStatus = null;
	    errorMessage = "Private 5 GHZ SSID is not enabled ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3:DESCRIPTION:Verify the 5 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 3: ACTION: Execute WebPA command to verify Private 5 GHz SSID enabled status");
	    LOGGER.info("STEP 3 :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Up\"");
	    LOGGER.info("**********************************************************************************");
	    // verify 5ghz radio status
	    radioStatus = radioSatusForAllBands
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
	    if (null != radioStatus) {
		errorMessage = "Verification of 5Ghz Private SSID status failed. EXPECTED ssid status is : "
			+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved " + radioStatus
			+ " value from WebPa command.";
		status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
	    } else {
		errorMessage = "Unable to fetch the 5Ghz Private SSID status using Webpa paramete"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successsfully Verified the 5 GHz SSID enabled status via WebPA");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4 Connect the device to 5 GHz SSID and verify connection status
	     */
	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "Connection to 5Ghz device failed";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: Description : Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("STEP 4: Action : Connect the client to 5 GHz SSID");
	    LOGGER.info("STEP 4: EXPECTED: Device should be connected with 5 GHz wifi network");
	    LOGGER.info("**********************************************************************************");
	    try {
		// get all connected devices and connect a device to 5Ghz
		connectedDeviceActivated = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
		status = null != connectedDeviceActivated;
	    } catch (Exception exception) {
		errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Device should be connected with 5 GHz wifi network");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Steps 5 to 8
	     */
	    BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
		    testId, new String[] { "s5", "s6", "s7", "s8" });
	    /**
	     * Steps 9 to 10
	     */
	    wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_8);

	} catch (Exception exception) {
	    LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
	    LOGGER.info("**********************************************************************************");
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);

	}

	LOGGER.info("ENDING TESTCASE : " + testId);

    }

    /**
     * Test to verify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is
     * disabled
     * <ol>
     * <li>STEP 1:Disable Private 5 GHz SSID via WebPA</li>
     * <li>STEP 2:Verify the Private 5 GHz SSID enabled status via WebPA</li>
     * <li>STEP 3:Verify the Private 2.4 GHz SSID enabled status via WebPA</li>
     * <li>STEP 4: Connect the device to 2.4 GHz SSID and verify connection status</li>
     * <li>STEP 5:Verify whether interface got the correct IPv4 address.</li>
     * <li>STEP 6:Verify whether interface got the correct IPv6 address.</li>
     * <li>STEP 7: Verify whether you have connectivity using that particular interface using IPV4</li>
     * <li>STEP 8: Verify whether you have connectivity using that particular interface using IPV6</li>
     * <li>STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value
     * of\"</li>
     * <li>STEP 10:Verify dmcli eRT getv Device.WiFi. - has to give more than 23 params.</li>
     * <li>Postcondition1:Revert WIFI SSID to its original state</li>
     * </ol>
     * 
     * @author anandam.s
     * @refactor Athira
     * @param device
     *            instance of {@link Dut}
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-4002")
    public void testToVerifyIPAddressWith5GHZDisabled(Dut device) {
	// String to store the test case status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-WIFI-402";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	String deviceDateTime = null;
	// connected device to be verified
	Dut connectedDeviceActivated = null;

	try {

	    LOGGER.info("STARTING TEST CASE: " + testId);

	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Test toVerify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is disabled");
	    LOGGER.info("STEP 1:Disable Private 5 GHz SSID via WebPA");
	    LOGGER.info("STEP 2:Verify the Private 5 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 3:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 4: Connect the device to 2.4 GHz SSID and verify connection status");
	    LOGGER.info("STEP 5:Verify whether interface got the correct IPv4  address.");
	    LOGGER.info("STEP 6:Verify whether interface got the correct IPv6  address.");
	    LOGGER.info("STEP 7: Verify whether you have connectivity using that particular interface using IPV4 ");
	    LOGGER.info("STEP 8: Verify whether you have connectivity using that particular interface using IPV6 ");
	    LOGGER.info(
		    "STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
	    LOGGER.info("STEP 10:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
	    LOGGER.info("Postcondition1:Revert WIFI SSID to its original state");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1:Description:Disable Private 5 GHz SSID via WebPA");
	    LOGGER.info("STEP 1:Action:Execute command Private 5 GHz SSID via WebPA "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    LOGGER.info(
		    "STEP 1: EXPECTED: Device should disable the Private 5 GHZ SSID and WebPA command return success response.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Private 5GHZ SSID is not disabled ";
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    // Disable 5ghz radio
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, false);
	    if (status) {
		LOGGER.info("STEP 1 : ACTUAL : Private 5 GHZ SSID disabled and WebPA command return success response");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    // wait for 1.5 min
	    LOGGER.info("Waiting for 1.5 minutes");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2:DESCRIPTION:Verify the Private 5 GHz SSID Disabled status via WebPA ");
	    LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 5GHz SSID disable status");
	    LOGGER.info("STEP 2 :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Down\"");
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s2";
	    status = false;
	    errorMessage = "Private 5 GHZ SSID is not disabled ";
	    // verify 5ghz radio status
	    Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
	    String radioStatus = radioSatusForAllBands
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
	    if (null != radioStatus) {
		status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
		if (status) {
		    LOGGER.info("STEP 2 : ACTUAL : Device returned the  Private 5 GHz SSID enabled status as Down");
		} else {
		    errorMessage = "Verification of 5Ghz Private SSID status failed. EXPECTED ssid status is : "
			    + BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
			    + radioStatus + " value from WebPa command.";
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
	    } else {
		errorMessage = "Unable to fetch the 5Ghz Private SSID status using Webpa paramete"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3:DESCRIPTION:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
	    LOGGER.info("STEP 3: ACTION: Execute WebPA command to verify Private 2.4GHz SSID enabled status");
	    LOGGER.info("STEP 3 :EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Up\"");
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "Private 2.4 GHZ SSID is not enabled ";
	    // verify 2.4ghz radio status
	    radioStatus = radioSatusForAllBands
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
	    if (null != radioStatus) {
		status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
		if (status) {
		    LOGGER.info("STEP 3 : ACTUAL : Device returned the  Private 2.4 GHz SSID enabled status as Up");
		} else {

		    errorMessage = "Verification of 2.4Ghz Private SSID status failed. EXPECTED ssid status is : "
			    + BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
			    + radioStatus + " value from WebPa command.";
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
	    } else {
		errorMessage = "Unable to fetch the 2.4Ghz Private SSID status using Webpa paramete"
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: Description : Connect the device to 2.4 GHz SSID and verify connection status");
	    LOGGER.info("STEP 4: Action : Connect the client to 2.4 GHz SSID");
	    LOGGER.info("STEP 4: EXPECTED: Device should be connected with 2.4 GHz wifi network");
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "Connection to 2.4Ghz device failed";

	    try {
		// get the 2.4ghz SSId and password of device device
		connectedDeviceActivated = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
		if (null != connectedDeviceActivated) {
		    status = true;
		} else {
		    errorMessage = "Unable to connect to 2.4GHz private SSID when 5GHZ Private SSID disabled mode";
		}
	    } catch (Exception exception) {
		errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /** Steps s5 to s8 */
	    BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
		    testId, new String[] { "s5", "s6", "s7", "s8" });
	    /**
	     * Steps 9 to 10
	     */
	    wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_8);

	} catch (Exception exception) {
	    LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
	    LOGGER.info("**********************************************************************************");
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);

	}

	LOGGER.info("ENDING TEST CASE: " + testId);

    }


	/**
	 * Test to verify that LAN side wireless CPE gets an IP address when dual radio
	 * bands(2.4GHZ and 5 GHZ) are enabled in CM
	 * 
	 * @param device instance of {@link Dut}
	 * @author Anandam S
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4003")
	public void testVerifyConnectivityOfClientDeviceWithGatewayEnabledWithBothPrivateSsid(Dut device) {
		boolean status = false;
		String testId = "TC-RDKB-WIFI-403";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		Dut connectedDeviceActivated = null;
		try {
			LOGGER.info("###################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-4003");
			LOGGER.info(
					"TEST DESCRIPTION: Verify that LAN side wireless CPE gets an IP address when dual radio bands(2.4GHZ and 5 GHZ) are enabled in CM");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Step 1: Verify the Private 2.4 GHz SSID enabled status via WebPA");
			LOGGER.info("Step 2: Verify the Private 5 GHz SSID enabled status via WebPA");
			LOGGER.info("Step 3: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("Step 4: Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step 5: Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step 6: Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 7: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("Step 8: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("Step 9: Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step 10: Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step 11: Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 12: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("###################################################################################");

			/**
			 * STEP 1 : VERIFY THE PRIVATE 2.4 GHZ SSID ENABLED STATUS
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : Verify the Private 2.4 GHz SSID enabled status");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : Verify the Private 2.4 GHz SSID enabled status using webpa.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPTECTED : Device should return the  Private 2.4 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 2.4 GHZ SSID status is not UP ";
			// verify 5ghz radio status
			Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
			String radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 2.4GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 2.4GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL :  Successfully Verified the 2.4 GHz ssid status and its enabled");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY THE PRIVATE 5 GHZ SSID ENABLED STATUS
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : Verify the Private 5 GHz SSID enabled status");
			LOGGER.info(
					"STEP :  " + stepNumber + " : ACTION : Verify the Private 5 GHz SSID enabled status using webpa.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPTECTED : Device should return the  Private 5 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 5 GHZ SSID status is not UP ";
			// verify 5ghz radio status
			radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 5GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 5GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL :  Successfully Verified the 5 GHz ssid status and its enabled");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3 : CONNECT THE CLIENT TO 2.4 GHZ
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			connectedDeviceActivated = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : Verify connecting the wi-fi client in the device to 2.4 GHz ssid");
			LOGGER.info(
					"STEP :  " + stepNumber + " : ACTION : Connect the wi-fi client with 2.4 GHz ssid and password");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : the connection must be successful");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Failed to connected the wifi client to 2.4 GHz ssid";
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (connectedDeviceActivated != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: Connected the wifi client to 2.4 GHz ssid successfully");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4-7 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED WITH
			 * PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);

			/**
			 * STEP 8 : CONNECT THE CLIENT TO 5 GHZ
			 */
			stepNumber = 8;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			connectedDeviceActivated = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : Verify connecting the wi-fi client in the device to 5ghz ssid");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : Connect the wi-fi client with 5ghz ssid and password");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : the connection must be successful");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Failed to connected the wifi client to 5ghz ssid";
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (connectedDeviceActivated != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9-12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6
			 * INTERFACE.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);
		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandPostConditionUtils.executePostConditionToVerifyDefaultRadioStatus(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-4003");
	}

	/**
	 * Verify the enable and disable functionality of both radios(2.4 GHz and 5 GHz)
	 * is working fine
	 * <ol>
	 * <li>STEP 1:Disable Private 2.4 GHz SSID via WebPA</li>
	 * <li>STEP 2:Verify the Private 2.4 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 3:Scan the broadcasting WI-Fi network from client device and check
	 * the connectivity status</li>
	 * <li>STEP 4:Disable Private 5 GHz SSID via WebPA</li>
	 * <li>STEP 5:Verify the Private 5 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 6:Scan the broadcasting WI-Fi network from client device and check
	 * the connectivity status</li>
	 * <li>STEP 7:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 8:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>STEP 9:Enable the Private 2.4 GHz SSID via WebPA</li>
	 * <li>STEP 10:Verify the Private 2.4 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 11: Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 12:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 13:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 14:Enable the Private 5 GHz SSID via WebPA</li>
	 * <li>STEP 15:Verify the Private 5 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 16: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 17:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 18:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 19:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 20:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li> *
	 * <li>Postcondition1:Revert WIFI SSID to its original state</li>
	 * </ol>
	 * 
	 * @author anandam.s
	 * 
	 * @param device {@link Device}
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4004")
	public void testVerifyConnectivityOfClientDeviceWhenEnableOrDisablingSsid(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-404";
		// Test step number
		String radioStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		String deviceDateTime = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		try {
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-4004");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test toVerify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is disabled");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("STEP 1:Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 2:Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info(
					"STEP 3:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info("STEP 4:Disable Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 5:Verify the Private 5 GHz SSID enabled status via WebPA ");
			LOGGER.info(
					"STEP 6:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info(
					"STEP 7:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 8:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP 9:Enable the Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 10:Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 11: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 12:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 13:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 14:Enable the Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 15:Verify the Private 5 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 16: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 17:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 18:Verify whether interface got the correct IPv6  address.");
			LOGGER.info(
					"STEP 19:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 20:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("#######################################################################################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION:Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 1: ACTION: Execute WebPA command to disable Private 2.4 GHz SSID");
			LOGGER.info(
					"STEP 1: EXPECTED: Device should disable the Private 2.4 GHZ SSID and WebPA command return success response.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 2.4GHZ SSID is not disabled ";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			// Disable 2.4ghz radio
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, false);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Private 2.4 GHz SSID is disabled via WebPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			radioStepNumber = "s2";
			status = false;
			errorMessage = "Private 2.4 GHZ SSID is not disabled ";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 2.4 GHz SSID disable status");
			LOGGER.info("STEP 2: EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Down\"");
			LOGGER.info("**********************************************************************************");
			// verify 5ghz radio status
			String radioStatus = BroadBandConnectedClientUtils.getRadioStatus(WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv,
					device);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 2GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 2GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Private 2.4 GHz SSID is in disable status");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			/** Wait for 4 mins for the changes to get applied */
			LOGGER.info("Waiting for 4 minutes");
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

			radioStepNumber = "s3";
			status = false;
			errorMessage = "Client device(s) is listed the Private 2.4 Ghz SSID Name in available network list";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info("STEP 3: ACTION: verify whether the 2.4Ghz SSID is not broadcasted");
			LOGGER.info(
					"STEP 3:EXPECTED: Client device(s) should not list the Private 2.4 Ghz SSID Name in available network list.");
			LOGGER.info("**********************************************************************************");
			boolean isVisible = false;
			String connectionType = null;
			errorMessage = "Client device(s) is listed the Private 2.4 Ghz SSID Name in available network list";
			// verify whether 2.4ghz ssid is visible in client devices
			List<Dut> connectedDevices = ((Device) device).getConnectedDeviceList();
			if (null != connectedDevices) {
				for (Dut connectedDevice : connectedDevices) {
					connectionType = ((Device) connectedDevice).getConnectedDeviceInfo().getConnectionType();
					if (CommonMethods.isNotNull(connectionType) && connectionType.trim().equalsIgnoreCase(
							BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
						isVisible = BroadBandConnectedClientUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(
								connectedDevice, BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
										device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ),
								tapEnv);
					}
					if (isVisible) {
						errorMessage = "2.4GHz SSID is listed in client device  model " + connectedDevice.getModel();
						LOGGER.info(errorMessage);
						break;
					}
				}
				status = !isVisible;
				if (status) {
					LOGGER.info("STEP 3: ACTUAL : 2.4GHz SSID is not listed in client device  model");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/**
				 * Step 4
				 */
				radioStepNumber = "s4";
				status = false;
				errorMessage = "Private 5GHZ SSID is not disabled ";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 4:DESCRIPTION:Disable Private 5 GHz SSID via WebPA");
				LOGGER.info("STEP 4: ACTION: Execute WebPA command to disable Private 5GHz SSID");
				LOGGER.info(
						"STEP 4:EXPECTED: Device should disable the Private 5 GHZ SSID and WebPA command return success response.");
				LOGGER.info("**********************************************************************************");
				// Disable 5ghz radio
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, false);
				if (status) {
					LOGGER.info("STEP 4: ACTUAL : Private 5GHz SSID is disabled via WebPA");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/**
				 * Step 5
				 */
				radioStepNumber = "s5";
				status = false;
				errorMessage = "Private 5 GHZ SSID is not disabled ";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 5:DESCRIPTION:Verify the Private 5 GHz SSID Disabled status via WebPA ");
				LOGGER.info("STEP 5: ACTION: Execute WebPA command to verify Private 5GHz SSID disable status");
				LOGGER.info(
						"STEP 5: :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Down\"");
				LOGGER.info("**********************************************************************************");
				// verify 5ghz radio status
				radioStatus = BroadBandConnectedClientUtils.getRadioStatus(WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv,
						device);
				if (null != radioStatus) {
					status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
					if (!status) {
						errorMessage = "Verification of 5GHz Private SSID status failed. EXPECTED ssid status is : "
								+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
								+ radioStatus + " value from WebPa command.";
					}
				} else {
					errorMessage = "Unable to fetch the 5GHz Private SSID status using Webpa parameter "
							+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
				}
				if (status) {
					LOGGER.info("STEP 5: ACTUAL : Private 5 GHz SSID is in disable status");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/** Wait for 4 mins for the changes to get applied */
				LOGGER.info("Waiting for 4 minutes");
				tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

				/**
				 * Step 6
				 */
				radioStepNumber = "s6";
				isVisible = false;
				errorMessage = "Client device(s) is listed the Private 5 Ghz SSID Name in available network list";
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 6:DESCRIPTION:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
				LOGGER.info("STEP 6: ACTION: verify whether the 5Ghz SSID is not broadcasted");
				LOGGER.info(
						"STEP 6:EXPECTED: Client device(s) should not list the Private 5 Ghz SSID Name in available network list.");
				LOGGER.info("**********************************************************************************");
				isVisible = false;
				// verify whether 5ghz ssid is visible in client devices
				for (Dut connectedDevice : connectedDevices) {
					connectionType = ((Device) connectedDevice).getConnectedDeviceInfo().getConnectionType();
					if (CommonMethods.isNotNull(connectionType) && connectionType.trim().equalsIgnoreCase(
							BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
						isVisible = BroadBandConnectedClientUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(
								connectedDevice, BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
										device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ),
								tapEnv);
					}
					if (isVisible) {
						errorMessage = "5GHz SSID is listed in client device  model " + connectedDevice.getModel();
						LOGGER.info(errorMessage);
						break;
					}
				}
				status = !isVisible;
				if (status) {
					LOGGER.info("STEP 6: ACTUAL : 5 GHz SSID is not listed in client device  model");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);
				/**
				 * Step 7-8
				 */
				wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_6);
				/**
				 * Step 9-13
				 */
				enableRadioAndCheckConnectivity(device, connectedDeviceActivated, testId,
						new String[] { "9", "10", "11", "12", "13" }, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				/**
				 * Step 14-18
				 */
				enableRadioAndCheckConnectivity(device, connectedDeviceActivated, testId,
						new String[] { "14", "15", "16", "17", "18" }, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				/**
				 * Step 19-20
				 */
				wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_18);
			} else {
				status = false;
				errorMessage = "Connected device list is empty";
				LOGGER.info(errorMessage);
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);
			}
		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, radioStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);
		}

		LOGGER.info("ENDING TESTCASE :testToVerifyIPAddressWith2GHZDisabled() ");
	}

	/**
	 * Method to check the dmcli parse fix of RDKB-28734
	 * 
	 * @param device     Dut instance
	 * @param tapEnv     instance of {@link AutomaticsTapApi}
	 * @param testCaseId test case id
	 * @param step       current stepNumber
	 * @return int returns current step number
	 * @Refactor Sruthi Santhosh
	 */
	public int wifiDmcliParseCheck(Dut device, AutomaticsTapApi tapEnv, String testCaseId, String deviceDateTime,
			int step) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		// String to store the error message
		String errorMessage = null;
		// response obtained
		String response = null;

		try {
			step++;
			String radioStepNumber = "s" + step;
			status = false;
			errorMessage = "\"Failed to get parameter value of\" is present in wifilog.txt";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step
					+ ": DESCRIPTION:Verify in WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\" ");
			LOGGER.info("STEP " + step
					+ " : ACTION: validate in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP " + step
					+ ": EXPECTED: \"Failed to get parameter value of\" is not present in /rdklogs/logs/WiFilog,txt.0");
			LOGGER.info("******************************************************");
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTestConstants.CONSTANT_WIFI_ERROR_MESSAGE,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);
			if (CommonMethods.isNull(response)) {
				status = true;
			} else {
				status = !BroadBandCommonUtils.verifyLogUsingTimeStamp(deviceDateTime, response);
			}
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  WiFilog.txt.0 doesn't contains any failure logs");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			step++;
			radioStepNumber = "s" + step;
			status = false;
			errorMessage = "\"Failed to get parameter value of\" is present in wifilog.txt";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step
					+ ": DESCRIPTION:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP " + step + " : ACTION: Execute following command  dmcli eRT getv Device.WiFi.");
			LOGGER.info("STEP " + step + ": Output should contain more than 23 param");
			LOGGER.info("******************************************************");
			int passCount = 0;
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_PARAMS);
			try {
				JSONArray responseInJson = new JSONArray(response);
				LOGGER.info("JSONResponse is " + responseInJson.length());
				int counter = 0;
				for (counter = 0; counter < responseInJson.length(); counter++) {
					JSONObject json = responseInJson.getJSONObject(counter);
					String name = json.getString(BroadBandTestConstants.STRING_NAME);
					if (CommonMethods.isNotNull(name)) {
						passCount++;
					}
				}
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
			}
			status = passCount != 0 && passCount > BroadBandTestConstants.CONSTANT_23;
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  Output has more than 23 param");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);
		} catch (Exception e) {
			LOGGER.error("Failed to check radio status" + e.getMessage());
			throw new TestException("Failed to check radio status" + errorMessage);
		}
		return step;
	}

	/**
	 * Steps to enable radio and check the IP connectivity
	 * 
	 * @param device
	 * @param connectedDeviceActivated
	 * @param testId
	 * @param stepNumbers
	 * @param radio
	 * @author Anandam s
	 * @Refactor Sruthi Santhosh
	 */
	private void enableRadioAndCheckConnectivity(Dut device, Dut connectedDeviceActivated, String testId,
			String[] stepNumbers, WiFiFrequencyBand radio) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		String radioStepNumber = "s" + stepNumbers[0];
		// String to store the error message
		String errorMessage = null;
		String radioTobeUsed;
		if (WiFiFrequencyBand.WIFI_BAND_2_GHZ.equals(radio)) {
			radioTobeUsed = "2.4Ghz";
		} else {
			radioTobeUsed = "5Ghz";
		}
		if (stepNumbers.length == 5) {

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumbers[0] + ":Enable Private " + radioTobeUsed + " SSID via WebPA");
			LOGGER.info("STEP " + stepNumbers[0] + ":EXPECTED : Device should enable the Private " + radioTobeUsed
					+ " GHZ SSID and WebPA command return success response.");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[0];
			status = false;
			errorMessage = "Private " + radioTobeUsed + " SSID is not enabled ";
			// enable given radio
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(radio, tapEnv,
					device, true);
			if (status) {
				LOGGER.info("STEP " + stepNumbers[0] + ":  ACTUAL RESULT : Enabled the Private" + radioTobeUsed
						+ " GHZ SSID and WebPA command return success response.");
			} else {
				LOGGER.info("STEP " + stepNumbers[0] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			LOGGER.info("Waiting for 1.5 minute");
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumbers[1] + ":Verify the Private " + radioTobeUsed
					+ "  SSID enabled status via WebPA ");
			LOGGER.info("STEP " + stepNumbers[1] + "EXPECTED: Device should return the  Private " + radioTobeUsed
					+ "  SSID enabled status as \"Up\"");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[1];
			status = false;
			errorMessage = "Private " + radioTobeUsed + " GHZ SSID is not enabled ";
			// verify radio status
			String radioStatus = BroadBandConnectedClientUtils.getRadioStatus(radio, tapEnv, device);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of " + radioTobeUsed
							+ " Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				String param = radioTobeUsed.contains("2.4")
						? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID
						: BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
				errorMessage = "Unable to fetch the " + radioTobeUsed + " Private SSID status using Webpa parameter "
						+ param;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumbers[1] + ":  ACTUAL RESULT :Verification of " + radioTobeUsed
						+ " Private SSID status is successful. SSID enabled status is Up.");
			} else {
				LOGGER.info("STEP " + stepNumbers[1] + ":  ACTUAL RESULT :" + errorMessage);
			}
			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP" + stepNumbers[2] + ": Connect the device to " + radioTobeUsed
					+ " SSID and verify connection status");
			LOGGER.info("STEP " + stepNumbers[2] + "EXPECTED: Device should be connected with " + radioTobeUsed
					+ " wifi network");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[2];
			status = false;
			errorMessage = "Connection to " + radioTobeUsed + " device failed";

			connectedDeviceActivated = radio.equals(WiFiFrequencyBand.WIFI_BAND_2_GHZ)
					? BroadBandConnectedClientUtils.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device,
							tapEnv)
					: BroadBandConnectedClientUtils.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device,
							tapEnv);
			if (null != connectedDeviceActivated) {
				status = true;
			} else {
				errorMessage = "Unable to connect to " + radioTobeUsed + " private SSID when " + radioTobeUsed
						+ " is enabled";
			}

			if (status) {
				LOGGER.info("STEP " + stepNumbers[2] + ":  ACTUAL RESULT :Able to connect to " + radioTobeUsed
						+ " private SSID when " + radioTobeUsed + " is enabled");
			} else {
				LOGGER.info("STEP " + stepNumbers[2] + ":  ACTUAL RESULT :" + errorMessage);
			}
			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			checkIpAddressObtained(device, tapEnv, connectedDeviceActivated, testId,
					new String[] { stepNumbers[3], stepNumbers[4] });
		} else {
			LOGGER.info("This function is meant for executing 5 steps.Current steps passed are " + stepNumbers.length);
		}
	}

	/**
	 * Common steps for checking IP address obtained for the connected client device
	 * 
	 * @param device
	 * @param connectedDeviceActivated
	 * @param testId
	 * @param stepNumbers
	 * @Refactor Sruthi Santhosh
	 */
	public static void checkIpAddressObtained(Dut device, AutomaticsTapApi tapEnv, Dut connectedDeviceActivated,
			String testId, String[] stepNumberss) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		String radioStepNumber = "s" + stepNumberss[0];
		long polling_window_ms = 90000L;
		// String to store the error message
		String errorMessage = null;
		if (stepNumberss.length == 2) {

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumberss[0] + ":Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP " + stepNumberss[0] + "EXPECTED: Interface IP address should be shown");
			LOGGER.info("******************************************************");

			errorMessage = "Interface did not get the correct IPV4 address";
			String osType = ((Device) connectedDeviceActivated).getOsType();
			long startTime = System.currentTimeMillis();
			radioStepNumber = "s" + stepNumberss[0];
			// Loop for this function is a waiting time of max 90sec for the
			// webpa changes to get applied
			do {
				status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				if (status) {
					break;
				}
			} while (System.currentTimeMillis() < (startTime + polling_window_ms));

			if (status) {
				LOGGER.info("STEP " + stepNumberss[0] + ":  ACTUAL RESULT : Interface got the correct IPv4 address.");
			} else {
				LOGGER.info("STEP " + stepNumberss[0] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, false);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumberss[1] + ":Verify whether interface got the correct IPv6  address.");
			LOGGER.info("EXPECTED " + stepNumberss[1] + ":Interface IP address should be shown");
			LOGGER.info("******************************************************");

			radioStepNumber = "s" + stepNumberss[1];
			status = false;
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					connectedDeviceActivated, tapEnv);

			if (status) {
				LOGGER.info("STEP " + stepNumberss[1] + ":  ACTUAL RESULT : Interface got the correct IPv6 address.");
			} else {
				LOGGER.info("STEP " + stepNumberss[1] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, false);
		} else {
			LOGGER.info("This function is meant for executing 2 steps.Current steps passed are " + stepNumberss.length);
		}
	}
	
	/**
     * This test case is to Verify whether WIFI supports WPA2-PSK-AES InValid-Key and valid-Key for 2.4Ghz
     * <ol>
     * <li>STEP 1:Execute WebPA command to Set on the security modes for 2.4ghz as-'WPA2-Personal' and validate whether
     * they are set correctly.</li>
     * 
     * <li>STEP 2:Connect the connected client device to 2.4GHz SSID and verify connection status by giving valid
     * password</li>
     *
     * <li>STEP 3: Verify whether interface got the correct IPv4 address.</li>
     * 
     * <li>STEP 4 :Verify whether interface got the correct IPv6 address.</li>
     * 
     * <li>STEP 5 :Verify whether you have connectivity using that particular interface using IPV4</li>
     *
     * <li>STEP 6 :Verify whether you have connectivity using that particular interface using IPV6</li>
     * 
     * <li>STEP 7 :Connect the connected client device to 2.4GHz SSID and verify connection status by giving Invalid
     * password</li>
     * 
     * </ol>
     * 
     * @param device
     *            Dut to be used
     * @author Joseph_Maduram
     * @refactor Said Hisham
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-SEC-MODE-1001")
    public void testVerifyWifiSecurityModeWithValidAndInvalidKeyFor2_4Ghz(Dut device) {
	boolean status = false;
	String testId = "TC-RDKB-WIFI-SEC-MODE-101";
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	String errorMessage = null;
	Dut connectedDeviceActivated = null;
	String ssidName = null;
	String command = null;
	String response = null;
	try {
	    LOGGER.info("###################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SEC-MODE-1001");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of 2.4 GHz Private SSID connectivity using valid and invalid WPA2-PSK-AES key");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Pre condition 1 : Verify the private wifi 2.4 GHz ssid is enabled");
	    LOGGER.info(
		    "Step : 1 Set on the security modes for 2.4ghz as-\"WPA2-Personal\" and validate whether they are set correctly.");
	    LOGGER.info(
		    "Step : 2 Connect the connected client device to 2.4GHz SSID and verify connection status by giving valid password");
	    LOGGER.info("Step : 3 Verify whether interface got the correct IPv4 address.");
	    LOGGER.info("Step : 4 Verify whether interface got the correct IPv6 address.");
	    LOGGER.info("Step : 5 Verify whether you have connectivity using that particular interface using IPV4");
	    LOGGER.info("Step : 6 Verify whether you have connectivity using that particular interface using IPV6");
	    LOGGER.info(
		    "Step : 7 Connect the connected client device to 2.4GHz SSID and verify connection status by giving Invalid password");
	    LOGGER.info("###################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    errorMessage = null;
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1 : DESCRIPTION : VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED, IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID ");
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTION : VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID USING WEBPA ");
	    LOGGER.info(
		    "PRE-CONDITION 1 : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 2.4 GHZ SSID AND RESPONSE SHOULD BE TRUE");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE";
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
			BroadBandTestConstants.TRUE);
	    } catch (TestException exception) {
		status = false;
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    if (!status) {
		errorMessage = "UNABLE TO SET THE PRIVATE 2.4 GHZ SSID STATUS AS 'TRUE'.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : PRIVATE 2.4 GHZ SSID VERIFIED/ENABLED IN GATEWAY DEVICE.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1:Execute WebPA command to Set on the security modes for 2.4ghz as-"WPA2-Personal" and validate
	     * whether they are set correctly.
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Set on the  security modes for 2.4 GHz as-'WPA2-Personal'  and validate whether they are set correctly.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Set the Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled with value \"WPA2-Personal\" using WebPA ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be able to set the parameter by WebPA");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to Set on the Wi-Fi  security modes for 2.4 GHz as-'WPA2-Personal via via WebPA Command 'Device.WiFi.AccessPoint.10001.Security.ModeEnabled'";
	    status = (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
		    WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Set on the  security modes for 2.4 GHz as-'WPA2-Personal");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2:Connect the connected client device to 2.4GHz SSID and verify connection status by giving valid
	     * password
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the connected client device  to 2.4 GHz SSID and verify connection status by giving valid password");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :  Connect to  2.4GHZ wifi using following  commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device should be connected with 2.4 GHz wifi network by giving valid password");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to connect to 2.4 GHz Private Wi-Fi network";
	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = (null != connectedDeviceActivated);
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfylly connected the client with 2.4GHz private ssid.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3: Verify whether the interface get the correct IPv4 address
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the interface get the correct IPv4 address.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Get the device IPv4 address using below command Linux : ifconfig wlan0 |grep -i \"inet addr:\" Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"Pv4 Address\"");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv4 address should be shown");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Interface  didnt get the correct IPV4 address";
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedDeviceActivated);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv4 address");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 4:Verify whether interface get the correct IPv6 address.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "interface  didnt got the correct IPV6 address";
	    String osType = ((Device) connectedDeviceActivated).getOsType();
	    status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
		    connectedDeviceActivated, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 5:Verify whether you have connectivity using that particular interface using IPV4.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV4 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : Linux :  curl -4 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -4 -v 'www.google.com' | grep '200 OK");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
	    LOGGER.info("***************************************************************************************");
	    command = ((Device) connectedDeviceActivated).getOsType()
		    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
			    ? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
			    : BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
	    errorMessage = "Connectivty check using IPV4 address failed";
	    response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
	    if (CommonMethods.isNotNull(response)) {
		status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
		if (!status) {
		    errorMessage = "Expected 200 OK as response .But obtained " + response;
		}
	    } else {
		errorMessage = "Unable to execute curl command for IPV4 on connected client device. Please check the connectivity between connected client and Jump server.";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV4");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 6:Verify whether there is no connectivity using that particular interface using IPV6.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV6 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : Linux :  curl -6 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -6 -v 'www.google.com' | grep '200 OK");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Connectivty check using IPV6 address failed";
	    command = ((Device) connectedDeviceActivated).getOsType()
		    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
			    ? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
			    : BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
	    response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
	    if (CommonMethods.isNotNull(response)) {
		status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
		if (!status) {
		    errorMessage = "Expected 200 OK as response .But obtained " + response;
		}
	    } else {
		errorMessage = "Unable to execute curl command for IPv6 on connected client device. Please check the connectivity between connected client and Jump server.";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV6");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 7:Connect the connected client device to 2.4 GHz SSID and verify connection status by giving Invalid
	     * password
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the connected client device  to 2.4 GHz SSID and verify connection status by giving Invalid password ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Connect to  2.4 GHZ wifi using below commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Device should not be connected with 2.4 GHz wifi network by giving the invalid password");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Connection to 2.4 Ghz  is successful even though by giving the invalid password";
	    ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    if (CommonMethods.isNotNull(ssidName)) {
		status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
			BroadbandPropertyFileHandler.getInvalidWifiPassword());
	    } else {
		errorMessage = "SSID name obtained for 2.4ghz is null";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Device is not  connected with 2.4 GHz wifi network by giving the invalid password");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-SEC-MODE-1001");
    }

    /**
     * This test case is to Verify whether WIFI supports WPA2-PSK-AES InValid-Key and valid-Key for 5Ghz
     * <ol>
     * 
     * <li>STEP 1:Execute WebPA command to Set on the security modes for 5ghz as-'WPA2-Personal' and validate whether
     * they are set correctly.</li>
     * 
     * <li>STEP 2: Connect the connected client device to 5GHz SSID and verify connection status by giving valid
     * password</li>
     * 
     * <li>STEP 3: Verify whether interface got the correct IPv4 address.</li>
     * 
     * <li>STEP 4 :Verify whether interface got the correct IPv6 address.</li>
     * 
     * <li>STEP 5 :Verify whether you have connectivity using that particular interface using IPV4</li>
     *
     * <li>STEP 6 :Verify whether you have connectivity using that particular interface using IPV6</li>
     * 
     * <li>STEP 7 :Connect the connected client device to 5GHz SSID and verify connection status by giving Invalid
     * password</li>
     * 
     * </ol>
     * 
     * @param device
     *            Dut to be used
     * @author Joseph_Maduram
     * @refactor Said Hisham
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-SEC-MODE-1002")
    public void testVerifyWifiSecurityModeWithValidAndInvalidKeyFor5Ghz(Dut device) {
	boolean status = false;
	String testId = "TC-RDKB-WIFI-SEC-MODE-102";
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	String errorMessage = null;
	Dut connectedDeviceActivated = null;
	String ssidName = null;
	String command = null;
	String response = null;
	try {
	    LOGGER.info("###################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SEC-MODE-1002");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify whether WIFI supports WPA2-PSK-AES InValid-Key and valid-Key for 5Ghz");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Pre condition 1 : Verify the private wifi 5 GHz ssid is enabled");
	    LOGGER.info(
		    "Step : 1 Set on the security modes for 5 GHz as-\"WPA2-Personal\" and validate whether they are set correctly.");
	    LOGGER.info(
		    "Step : 2 Connect the connected client device to 5 GHz SSID and verify connection status by giving valid password");
	    LOGGER.info("Step : 3 Verify whether interface got the correct IPv4 address.");
	    LOGGER.info("Step : 4 Verify whether interface got the correct IPv6 address.");
	    LOGGER.info("Step : 5 Verify whether you have connectivity using that particular interface using IPV4");
	    LOGGER.info("Step : 6 Verify whether you have connectivity using that particular interface using IPV6");
	    LOGGER.info(
		    "Step : 7 Connect the connected client device to 5 GHz SSID and verify connection status by giving Invalid password");
	    LOGGER.info("###################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1 : DESCRIPTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID ");
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID USING WEBPA ");
	    LOGGER.info(
		    "PRE-CONDITION 1 : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 5 GHZ SSID AND RESPONSE SHOULD BE TRUE");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE";
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED,
			BroadBandTestConstants.TRUE);
	    } catch (TestException exception) {
		status = false;
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    if (!status) {
		errorMessage = "UNABLE TO SET THE PRIVATE 5 GHZ SSID STATUS AS 'TRUE'.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : PRIVATE 5 GHZ SSID VERIFIED/ENABLED IN GATEWAY DEVICE.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1:Execute WebPA command to Set on the security modes for 5ghz as-"WPA2-Personal" and validate
	     * whether they are set correctly.
	     * 
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Set on the  security modes for 5 GHz as-'WPA2-Personal'  and validate whether they are set correctly.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Set the Parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled with value \"WPA2-Personal\" using WebPA ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be able to set the parameter by WebPA");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to Set on the Wi-Fi  security modes for 5 GHz as-'WPA2-Personal' via WebPA Command 'Device.WiFi.AccessPoint.10101.Security.ModeEnabled'";
	    status = (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
		    WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Set on the  security modes for 5 GHz as-'WPA2-Personal");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 2: Connect the connected client device to 5GHz SSID and verify connection status by giving valid
	     * password
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the connected client device  to 5 GHz SSID and verify connection status by giving valid password");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :  Connect to  5 GHZ wifi using following  commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device should be connected with 5 GHz wifi network by giving valid password");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to connect to 5 GHz Private Wi-Fi network.";
	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    status = (null != connectedDeviceActivated);
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfylly connected the client with 5 GHz private ssid.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * STEP 3: Verify whether the interface get the correct IPv4 address
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the interface get the correct IPv4 address.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Get the device IPv4 address using below command Linux : ifconfig wlan0 |grep -i \"inet addr:\" Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"Pv4 Address\"");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv4 address should be shown");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Interface  didnt get the correct IPV4 address";
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedDeviceActivated);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv4 address");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 4:Verify whether interface get the correct IPv6 address.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "interface  didnt got the correct IPV6 address";
	    String osType = ((Device) connectedDeviceActivated).getOsType();
	    status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
		    connectedDeviceActivated, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 5:Verify whether you have connectivity using that particular interface using IPV4.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV4 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : Linux :  curl -4 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -4 -v 'www.google.com' | grep '200 OK");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
	    LOGGER.info("***************************************************************************************");
	    command = ((Device) connectedDeviceActivated).getOsType()
		    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
			    ? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
			    : BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
	    errorMessage = "Connectivty check using IPV4 address failed";
	    response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
	    if (CommonMethods.isNotNull(response)) {
		status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
		if (!status) {
		    errorMessage = "Expected 200 OK as response .But obtained " + response;
		}
	    } else {
		errorMessage = "Unable to execute curl command for IPV4 on connected client device. Please check the connectivity between connected client and Jump server.";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV4");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 6:Verify whether there is no connectivity using that particular interface using IPV6.
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV6 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command : Linux :  curl -6 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -6 -v 'www.google.com' | grep '200 OK");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Connectivty check using IPV6 address failed";
	    command = ((Device) connectedDeviceActivated).getOsType()
		    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
			    ? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
			    : BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
	    response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
	    if (CommonMethods.isNotNull(response)) {
		status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
		if (!status) {
		    errorMessage = "Expected 200 OK as response .But obtained " + response;
		}
	    } else {
		errorMessage = "Unable to execute curl command for IPv6 on connected client device. Please check the connectivity between connected client and Jump server.";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV6");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 7:Connect the connected client device to 5 GHz SSID and verify connection status by giving Invalid
	     * password
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the connected client device  to 5 GHz SSID and verify connection status by giving Invalid password ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Connect to  5 GHZ wifi using below commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Device should not be connected with 5 GHz wifi network by giving the invalid password");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Connection to 5 Ghz  is successful even though by giving invalid password";
	    ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    if (CommonMethods.isNotNull(ssidName)) {
		status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
			BroadbandPropertyFileHandler.getInvalidWifiPassword());
	    } else {
		errorMessage = "SSID name obtained for 5Ghz is null";
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Device is not  connected with 5 GHz wifi network by giving the invalid password");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-SEC-MODE-1002");
    }

    /**
     * Automate connecting client to 5 GHz SSID when 2.4 GHz SSID is disabled
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Pre-Condition : Disable and Verify 2.4GHz SSID<>
     * <li>Step 1: Verify 5 GHz frequency enabled status</li>
     * <li>Step 2: Connect & verify the client device into 5 GHz frequency SSID</li>
     * <li>Step 3: Verify whether interface got the correct IPv4 address.</li>
     * <li>Step 4: Verify whether interface got the correct IPv6 address.</li>
     * <li>Step 5: Verify whether connectivity using that particular interface using IPv4 Address</li>
     * <li>Step 6: Verify whether connectivity using that particular interface using IPv6 Address</li>
     * </ol>
     * 
     * @author Susheela C
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-WIFI-2050")
    public void testVerifyBroadBandWifiSsidStatusFor2GHz(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-250";
	boolean status = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    /**
	     * Pre-condition : Disable and Verify 2.4GHz SSID
	     */
	    if (!BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
		    WebPaDataTypes.BOOLEAN.getValue(), RDKBTestConstants.FALSE, RDKBTestConstants.THREE_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		throw new TestException(
			"Pre-Condition error : Failed to disable 2.4GHz private SSID on the router device");
	    }
	    /**
	     * Step 1: Verify 5 GHz frequency enabled status via WebPA
	     */
	    step = "s1";
	    status = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify 5 GHz frequency enabled status via WebPA");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute command  : curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA_URL><ECM_MAC>/config?names=Device.WiFi.SSID.2.Status");
	    LOGGER.info("STEP 1: EXPECTED : Value must return true for 5 GHz");
	    LOGGER.info("*****************************************************************************************");
	    errorMessage = "Failed to validate/enable the SSID status for 5GHz Wifi network";
	    /**
	     * Get the value of webpa param "Device.WiFi.SSID.10101.Enable", if true move to step 2 or enable the SSID
	     */
	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    RDKBTestConstants.TRUE);
	    if (!status) {
		// Enabling the 5GHz private SSID on the router device
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			WebPaDataTypes.BOOLEAN.getValue(), RDKBTestConstants.TRUE, RDKBTestConstants.THREE_MINUTES,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : The SSID enable status value is true for 5GHz WiFi network");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("Actual: "
		    + (status ? "The SSID enable status value is true for 5GHz WiFi network" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    /**
	     * Step 2: Connect & verify the client device into 5 GHz frequency SSID.
	     */
	    step = "s2";
	    status = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Connect & verify the client device into 5 GHz frequency SSID");	  
	    LOGGER.info(
		    "STEP 2: ACTION : Connected a 5GHz wifi client with the gateway device's 5GHz wifi network using SSID and PASSWORD");
	    LOGGER.info("STEP 2: EXPECTED : Connected client must connect with 5 GHz frequency SSID");
	    LOGGER.info("*****************************************************************************************");
	    errorMessage = "Unable to Connect & verify the client device into 5 GHz SSID";
	    // Connect Windows / Linuz client to 5 GHz wifi ssid
	    Dut clientSettop = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    status = null != clientSettop;
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully connected the client to 5GHz wifi network");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info(
		    "Actual: " + (status ? "Successfully connected the client to 5GHz wifi network" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    // Waiting for 2 min after connecting a client
	    LOGGER.info("Waiting for two minutes after connecting a client...");
	    tapEnv.waitTill(RDKBTestConstants.TWO_MINUTES);
	    /**
	     * Steps 3 to 6
	     */
	    BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, clientSettop, testCaseId,
		    new String[] { "s3", "s4", "s5", "s6" });
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while enabling/ disabling the SSID Advertisement value for 5 GHz network"
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    // Enable the WebPA parameter
	    // "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
	}
    }


}